import chisel3._
import chisel3.util._
import org.scalacheck.Prop.{False, all}

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

    val colsTester = Output(Vec(60, UInt(8.W)))
    val aboveBlackTester = Output(Bool())

  })


  //State enum and register
  val idle :: borderWalls :: init :: getPixels :: top :: writeColors :: getColors :: finish :: bottom :: done :: Nil = Enum (10)
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val x = RegInit(0.U(8.W))
  val y = RegInit(0.U(8.W))
  val color = RegInit(0.U(8.W))
  val cols = RegInit(VecInit(Seq.fill(40)(1.U(8.W))))
  val colRight = RegInit(VecInit(Seq.fill(20)(1.U(8.W))))
  val colsize = 20.U
  val mNum = RegInit(0.U(2.W))
  val colorsToCheck = RegInit(VecInit(Seq.fill(5)(0.S(6.W))))
  val checkCount = RegInit(0.U(4.W))
  val backCount = RegInit(0.U(5.W))
  val colorsToWrite = RegInit(VecInit(Seq.fill(3)(0.U(8.W))))
  val endCheck = Wire(Bool())
  val needsCheckCountArr = Wire(Vec(5, UInt(8.W)))
  val aboveBlack = Wire(Bool())
  val checkCol = Wire(Bool())
  val nextWhite = RegInit(false.B)
  val shouldEnd = RegInit(false.B)
  endCheck := false.B
  needsCheckCountArr := VecInit(Seq.fill(5)(0.U))
  io.colsTester := cols ++ colRight
  io.aboveBlackTester := false.B

  aboveBlack := false.B
  nextWhite := false.B
  checkCol := false.B
  shouldEnd := false.B

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := 0.U
  io.done := false.B


  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := init
        addressReg := 0.U(16.W)
      }
    }

    is(init) {
      io.address := 400.U
      io.dataWrite := 0.U
      io.writeEnable := true.B
      y := 1.U
      x := 0.U
      stateReg := borderWalls
    }

    is(borderWalls) {
      y := y + 1.U
      io.address := y*20.U + x + 400.U
      io.writeEnable := true.B
      io.dataWrite := 0.U
      stateReg := borderWalls

      when(y === 19.U){
        when(x === 19.U){
          x := 1.U
          y := 19.U
          stateReg := bottom
        }.otherwise{
          stateReg := borderWalls
          y := 0.U
          x := 19.U

        }
      }
    }

    is(bottom){
      x := x + 1.U
      io.address := y*20.U + x + 400.U
      io.writeEnable := true.B
      io.dataWrite := 0.U
      stateReg := bottom
      when (x === 18.U) {
        x := 1.U
        y := 0.U
        stateReg := top
      }
    }

    is(top){
      x := x + 1.U
      io.address := y*20.U + x + 400.U
      io.writeEnable := true.B
      io.dataWrite := 0.U
      stateReg := top
      when (x === 18.U) {
        stateReg := getPixels
        addressReg := 3.U * 20.U + 1.U
        x := 1.U
        y := 3.U
      }
    }

    is(getPixels) {
      when(nextWhite || (cols(colsize + y) =/= 1.U)) {
        colRight(y) := io.dataRead
        io.address := addressReg + 1.U
      }.otherwise {
        cols(y + colsize) := io.dataRead
        io.address := addressReg
      }

      val colsPrev = cols(colsize + y - 1.U)
      val colsPrev2 = cols(colsize + y - 2.U)
      val colsNext = cols(colsize + y + 1.U)

      when(colsPrev === 0.U) {
        colorsToWrite.foreach(_ := 0.U)
        stateReg := writeColors
      }.elsewhen(nextWhite && io.dataRead === 0.U) {
        colorsToWrite(0) := 0.U
        backCount := 1.U

        when(colsPrev2 === 0.U) {
          stateReg := writeColors
          colorsToWrite.foreach(_ := 0.U)
        }.elsewhen(cols(y - 1.U) === 0.U) {
          colorsToWrite(1) := 0.U
          backCount := 2.U
          stateReg := getColors
        }.otherwise {
          needsCheckCountArr.foreach(_ := 0.U)
          when(colsPrev2 === 1.U) {
            colorsToCheck(needsCheckCountArr(0)) := -20.S
            needsCheckCountArr(1) := needsCheckCountArr(0) + 1.U
          }
          needsCheckCountArr(2) := needsCheckCountArr(1)
          when(cols(y - 1.U) === 1.U) {
            colorsToCheck(needsCheckCountArr(1)) := -1.S
            needsCheckCountArr(2) := needsCheckCountArr(1) + 1.U
          }
          needsCheckCountArr(3) := needsCheckCountArr(2)
          colorsToCheck(needsCheckCountArr(3)) := 1.S
          needsCheckCountArr(4) := needsCheckCountArr(3) + 1.U
          colorsToCheck(needsCheckCountArr(4)) := 0.S
          stateReg := getColors
        }
      }.elsewhen((io.dataRead === 255.U && !nextWhite) || nextWhite) {
        color := 255.U
        backCount := 0.U
        stateReg := getColors
        needsCheckCountArr.foreach(_ := 0.U)

        when(colsPrev === 1.U) {
          colorsToCheck(needsCheckCountArr(0)) := -20.S
          needsCheckCountArr(1) := needsCheckCountArr(0) + 1.U
        }
        needsCheckCountArr(2) := needsCheckCountArr(1)
        when(colsNext === 1.U && io.dataRead =/= 0.U && cols(y) =/= 0.U) {
          colorsToCheck(needsCheckCountArr(1)) := 20.S
          needsCheckCountArr(2) := needsCheckCountArr(1) + 1.U
        }
        needsCheckCountArr(3) := needsCheckCountArr(2)
        when(cols(y) === 1.U && io.dataRead =/= 0.U && colsNext =/= 0.U) {
          colorsToCheck(needsCheckCountArr(2)) := -1.S
          needsCheckCountArr(3) := needsCheckCountArr(2) + 1.U
        }
        needsCheckCountArr(4) := needsCheckCountArr(3)
        when(!nextWhite && colsNext =/= 0.U && cols(y) =/= 0.U) {
          colorsToCheck(needsCheckCountArr(3)) := 1.S
          needsCheckCountArr(4) := needsCheckCountArr(3) + 1.U
        }

        colorsToCheck(needsCheckCountArr(4)) := 0.S

        when(cols(y) === 0.U || colsNext === 0.U || colsPrev === 0.U || colRight(y) === 0.U) {
          colorsToWrite(0) := 0.U
          needsCheckCountArr.foreach(_ := 0.U)
          needsCheckCountArr(1) := needsCheckCountArr(0)
          when(cols(y - 1.U) === 1.U) {
            colorsToCheck(needsCheckCountArr(0)) := -20.S
            needsCheckCountArr(1) := needsCheckCountArr(0) + 1.U
          }
          needsCheckCountArr(2) := needsCheckCountArr(1)
          when(cols(y + colsize - 2.U) === 1.U) {
            colorsToCheck(needsCheckCountArr(1)) := -1.S
            needsCheckCountArr(2) := needsCheckCountArr(1) + 1.U
          }
          colorsToCheck(needsCheckCountArr(2)) := 1.S
          needsCheckCountArr(3) := needsCheckCountArr(2) + 1.U
          colorsToCheck(needsCheckCountArr(3)) := 0.S
          backCount := 1.U
        }
      }.elsewhen(cols(y - 3.U + colsize) === 255.U) {
        backCount := 0.U
        stateReg := getColors
        needsCheckCountArr.foreach(_ := 0.U)
        when(colsPrev === 1.U) {
          colorsToCheck(needsCheckCountArr(0)) := -20.S
          needsCheckCountArr(1) := needsCheckCountArr(0) + 1.U
        }
        needsCheckCountArr(2) := needsCheckCountArr(1)
        when(colsNext === 1.U && io.dataRead =/= 0.U && cols(y) =/= 0.U) {
          colorsToCheck(needsCheckCountArr(1)) := 20.S
          needsCheckCountArr(2) := needsCheckCountArr(1) + 1.U
        }
        needsCheckCountArr(3) := needsCheckCountArr(2)
        when(cols(y) === 1.U && io.dataRead =/= 0.U && colsNext =/= 0.U) {
          colorsToCheck(needsCheckCountArr(2)) := -1.S
          needsCheckCountArr(3) := needsCheckCountArr(2) + 1.U
        }
        needsCheckCountArr(4) := needsCheckCountArr(3)
        when(!nextWhite && colsNext =/= 0.U && cols(y) =/= 0.U) {
          colorsToCheck(needsCheckCountArr(3)) := 1.S
          needsCheckCountArr(4) := needsCheckCountArr(3) + 1.U
        }
        colorsToCheck(needsCheckCountArr(4)) := 0.S

        when(colsPrev === 0.U) {
          stateReg := writeColors
          color := 0.U
        }
      }.otherwise {
        stateReg := writeColors
        color := 0.U
      }
      colorsToWrite.foreach(_ := 0.U)
    }

    is(getColors) {
      io.address := (addressReg.zext() + (colorsToCheck(checkCount) - 20.S*backCount.zext())).asUInt()
      endCheck := false.B

      when(io.dataRead === 0.U) {
        endCheck := true.B
      }
      aboveBlack := false.B
      when(colorsToCheck(checkCount) === -20.S) {
        aboveBlack := io.dataRead === 0.U
        cols(colsize + y - 1.U - backCount) := io.dataRead
      } .elsewhen(colorsToCheck(checkCount) === 20.S) {
        cols(colsize + y + 1.U - backCount) := io.dataRead
      } .elsewhen(colorsToCheck(checkCount) === 1.S) {
        colRight(y - backCount) := io.dataRead
      }
      when(backCount === 0.U){
        when(cols(colsize + y) === 0.U){
          checkCol := true.B
        }.elsewhen(cols(y) === 0.U){
          checkCol := true.B
        }.elsewhen(colRight(y) === 0.U){
          checkCol := true.B
        }
        when(checkCol === true.B){
          color := 0.U
          endCheck := true.B
        }.otherwise {
          color := color & io.dataRead
        }
      }.otherwise {
        color := color & io.dataRead
      }

      io.aboveBlackTester := aboveBlack
      colorsToCheck(checkCount) := 0.S
      checkCount := checkCount + 1.U
      stateReg := getColors
      when((aboveBlack || cols(y -1.U + colsize) === 0.U) && backCount === 0.U) {
        checkCount := 0.U
        colorsToWrite.foreach(_ := 0.U)
        stateReg := writeColors
      }.elsewhen(backCount === 1.U && aboveBlack){
        colorsToWrite(1) := 0.U
        colorsToWrite(2) := 0.U
        stateReg := writeColors
        backCount := 0.U
        checkCount := 0.U
      }.elsewhen (colorsToCheck(checkCount+1.U) === 0.S || endCheck || checkCount+1.U === 4.U) {
        checkCount := 0.U
        colorsToWrite(backCount) := Mux(endCheck, 0.U, color & io.dataRead)
        color := 255.U
        backCount := backCount + 1.U
        needsCheckCountArr.foreach(_ := 0.U)

        when(cols(colsize + y - 2.U - backCount) === 1.U) {
          colorsToCheck(needsCheckCountArr(0)) := -20.S
          needsCheckCountArr(1) := needsCheckCountArr(0) + 1.U
        }
        needsCheckCountArr(2) := needsCheckCountArr(1)

        when(cols(y - 1.U - backCount) === 1.U) {
          colorsToCheck(needsCheckCountArr(1)) := -1.S
          needsCheckCountArr(2) := needsCheckCountArr(1) + 1.U
        }
        colorsToCheck(needsCheckCountArr(2)) := 1.S
        needsCheckCountArr(3) := needsCheckCountArr(2) + 1.U
        colorsToCheck(needsCheckCountArr(3)) := 0.S
        stateReg := getColors

        when(backCount === 0.U){
          when(cols(y) === 0.U || colRight(y) === 0.U) {
            colorsToWrite(0) := 0.U
          }

          when(cols(colsize + y - 2.U) === 0.U) {
            colorsToWrite(1) := 0.U
            colorsToWrite(2) := 0.U
            stateReg := writeColors
          }

          when(cols(y + colsize) === 0.U) {
            colorsToWrite(1.U) := 0.U
            needsCheckCountArr.foreach(_ := 0.U)
            needsCheckCountArr(1) := needsCheckCountArr(0)
            when(cols(y + colsize - 1.U) === 1.U) {
              colorsToCheck(needsCheckCountArr(0)) := -20.S
              needsCheckCountArr(1) := needsCheckCountArr(0) + 1.U
            }
            needsCheckCountArr(2) := needsCheckCountArr(1)
            when(cols(y - 2.U) === 1.U) {
              colorsToCheck(needsCheckCountArr(1)) := -1.S
              needsCheckCountArr(2) := needsCheckCountArr(1) + 1.U
            }
            colorsToCheck(needsCheckCountArr(2)) := 1.S
            needsCheckCountArr(3) := needsCheckCountArr(2) + 1.U
            colorsToCheck(needsCheckCountArr(3)) := 0.S
            backCount := 2.U
          }
          when(cols(y-1.U) === 0.U) {
            colorsToWrite(1) := 0.U
            needsCheckCountArr := VecInit(Seq.fill(5)(0.U))
            when(cols(y - 2.U) === 1.U) {
              colorsToCheck(needsCheckCountArr(0)) := -1.S
              needsCheckCountArr(1) := needsCheckCountArr(0) + 1.U
            }
            when(cols(y - 2.U) === 0.U || cols(y + colsize - 3.U) === 0.U) {
              colorsToWrite(2) := 0.U
              stateReg := writeColors
            }
            colorsToCheck(needsCheckCountArr(1)) := 1.S
            needsCheckCountArr(2) := needsCheckCountArr(1) + 1.U
            colorsToCheck(needsCheckCountArr(2)) := 0.S
          }
        }.elsewhen (backCount === 1.U) {
          when ((cols(y - 2.U) === 0.U || cols(y + colsize - 3.U) === 0.U) && backCount === 1.U) {
            colorsToWrite(2) := 0.U
            stateReg := writeColors
          }
        }.otherwise {
          backCount := 0.U
          stateReg := writeColors
        }
      }
    }

    is(writeColors) {
      io.address := addressReg + 400.U  - (20.U*mNum)
      io.dataWrite := colorsToWrite(mNum)
      io.writeEnable := true.B

      cols(y-1.U - mNum) := cols(y-1.U - mNum + colsize)
      cols(y-1.U -mNum + colsize) := colRight(y-1.U - mNum)
      colRight(y-1.U - mNum) := 1.U

      mNum := mNum + 1.U
      when (mNum === 2.U) {
        mNum := 0.U
        stateReg := getPixels
        backCount := 0.U

        when(cols(colsize + y+3.U) === 255.U) {
          nextWhite := true.B
        } .elsewhen (cols(colsize + y+3.U) === 0.U) {
          colorsToWrite.foreach(_ := 0.U)
          stateReg := writeColors
        }

        addressReg := (y+3.U)*20.U + x
        y := y + 3.U

        when(y === 18.U) {
          addressReg := 3.U*20.U + x + 1.U
          for (i <- 0 until 2) {
            cols(y + i.U) := cols(y + colsize + i.U)
            cols(y + colsize + i.U) := colRight(y + i.U)
            colRight(y + i.U) := 1.U
          }
          x := x + 1.U
          y := 3.U

          when(x === 18.U){
            stateReg := done
          }
        }
      }
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }
}
