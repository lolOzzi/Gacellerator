import chisel3._
import chisel3.util._
import org.scalacheck.Prop.False

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))


  })


  //State enum and register
  val idle :: write :: getColor :: writeColors :: getPixel :: writeMissedBlack :: finish :: bottom :: done :: Nil = Enum (9)
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val x = RegInit(0.U(8.W))
  val y = RegInit(0.U(8.W))
  val lastWhite = RegInit(0.U(1.W))
  val color = RegInit(0.U(8.W))
  val colorNext = RegInit(0.U(8.W))
  val cols = RegInit(VecInit(Seq.fill(40)(1.U(8.W))))
  val colsize = 20.U
  val init = RegInit(0.U(1.W))
  val addressRegNext = RegInit(0.U(16.W))
  val addressCheck = RegInit(0.U(16.W))
  val skipCount = RegInit(2.U(16.W))
  val mNum = RegInit(0.U(2.W))
  val checkedColors = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
  val colorsToCheck = RegInit(VecInit(Seq.fill(4)(0.S(6.W))))
  val checkCount = RegInit(0.U(4.W))
  val backCount = RegInit(0.U(5.W))
  val colorsToWrite = RegInit(VecInit(Seq.fill(3)(0.U(8.W))))
  val endCheck = Wire(Bool())
  val needsCheckCount = Wire(0.U(4.W))
  endCheck := false.B
  needsCheckCount := 0.U

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := 0.U
  io.done := false.B

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := write
        addressReg := 0.U(16.W)
      }
    }

    is(write) {

      when (init === 1.U) {
        io.address := addressReg + 400.U
        io.dataWrite := color
        io.writeEnable := true.B

      }. otherwise {
        init := 1.U
      }

      stateReg := getColor
      colorNext := 0.U

      when (x === 20.U) {
        stateReg := done
      }
      y := y + 3.U

    }
    is(writeMissedBlack) {
      io.address := addressReg + 400.U  - (20.U*mNum)
      io.dataWrite := 0.U
      io.writeEnable := true.B
      when (mNum === 0.U) {
        io.address := addressReg + 400.U
        io.dataWrite := color
        mNum := 1.U
        stateReg := writeMissedBlack
      } .elsewhen(mNum === 1.U) {
        mNum := 2.U
        stateReg := writeMissedBlack
        y := y + 3.U
        when(y >= 18.U) {
          x := x + 1.U
          y := 0.U
        }
      }.otherwise {
        mNum := 0.U
        stateReg := getColor
        addressReg := y*20.U + x
      }
      io.dataWrite := 0.U
    }
    is(writeColors) {
    }

    is(getColor) {

      io.address := addressReg
      cols(y + colsize) := io.dataRead
      when(io.dataRead === 255.U) {
        colorNext := io.dataRead
        stateReg := getPixel
        needsCheckCount := 0.U
        when(cols(colsize + y - 1.U) === 1.U) {
          colorsToCheck(needsCheckCount) := -20.S
          needsCheckCount := needsCheckCount + 1.U
        }
        when(cols(colsize + y + 1.U) === 1.U) {
          colorsToCheck(needsCheckCount) := 20.S
          needsCheckCount := needsCheckCount + 1.U
        }
        when(cols(y) === 1.U) {
          colorsToCheck(needsCheckCount) := -1.S
          needsCheckCount := needsCheckCount + 1.U
        }
        colorsToCheck(needsCheckCount) := 1.S
        needsCheckCount := needsCheckCount + 1.U
        colorsToCheck(needsCheckCount) := 0.S

        when(cols(colsize + y - 1.U) === 0.U) {
          stateReg := writeMissedBlack
          color := 0.U
        }


      } .otherwise {
        stateReg := writeMissedBlack
        color := 0.U
      }

      when (y === 3.U) {
        addressReg := y*20.U + x
      }
      when (y === 0.U) {
        stateReg := write
        color := 0.U
      }
      when(x === 20.U) {
        x := 0.U
        y := 19.U
        stateReg := bottom
      }
    }
    is(getPixel) {

      io.address := addressReg + (colorsToCheck(checkCount) - backCount.asSInt())
      endCheck := false.B

      when(colorsToCheck(checkCount+1.U) =/= 0.S) {

        when(io.dataRead === 0.U) {
          endCheck := true.B
        }

        when(colorsToCheck(checkCount) === -20.S) {
          cols(colsize + y-1.U - backCount) := io.dataRead
        } .elsewhen(colorsToCheck(checkCount) === 20.S) {
          cols(colsize + y +1.U - backCount) := io.dataRead
        } .elsewhen(colorsToCheck(checkCount) === 1.S) {
          cols(colsize + y - backCount) := io.dataRead
        }

        color := color & io.dataRead
        colorsToCheck(checkCount) := 0.U
        checkedColors(checkCount) := io.dataRead
        checkCount := checkCount + 1.U
        stateReg := getPixel
      }
      when (colorsToCheck(checkCount+1.U) === 0.S || endCheck) {
        checkCount := 0.U

        colorsToWrite(backCount) := Mux(endCheck, 0.U, color)
        when (backCount < 2.U) {
          backCount := backCount + 1.U
          needsCheckCount := 0.U
          when(cols(colsize + y  - 2.U - backCount) === 1.U) {
            colorsToCheck(needsCheckCount) := -20.S
            needsCheckCount := needsCheckCount + 1.U
          }
          when(cols(y -1.U -backCount) === 1.U) {
            colorsToCheck(needsCheckCount) := -1.S
            needsCheckCount := needsCheckCount + 1.U
          }
          when(backCount === 0.U) {
            colorsToCheck(needsCheckCount) := 1.S
            needsCheckCount := needsCheckCount + 1.U
          }
          colorsToCheck(needsCheckCount) := 0.S
          stateReg := getPixel

          when(cols(colsize + y - 2.U) === 0.U && backCount === 0.U) {
            colorsToWrite(1) := 0.U
            colorsToWrite(2) := 0.U
            stateReg := writeColors
          }
          when ((cols(y - 2.U) === 0.U || cols(y + colsize - 3.U) === 0.U) && backCount === 1.U) {
            colorsToWrite(2) := 0.U
            stateReg := writeColors
          }
          when(cols(y - 1.U) === 0.U && backCount === 0.U) {
            colorsToWrite(1) := 0.U
            needsCheckCount := 0.U
            when(cols(y - 2.U) === 1.U) {
              colorsToCheck(needsCheckCount) := -1.S
              needsCheckCount := needsCheckCount + 1.U
            }
            when(cols(y - 2.U) === 0.U || cols(y + colsize - 3.U) === 0.U) {
              colorsToWrite(1) := 0.U
              colorsToWrite(2) := 0.U
              stateReg := writeColors
            }
            colorsToCheck(needsCheckCount) := 1.S
            needsCheckCount := needsCheckCount + 1.U
            colorsToCheck(needsCheckCount) := 0.S
          }



        }
      }

    }


    is(bottom){
      x := x + 1.U
      io.address := y*20.U + x + 400.U
      io.writeEnable := true.B
      io.dataWrite := 0.U
      stateReg := bottom
      when (x === 20.U) {
        io.address := 400.U
        stateReg := done
      }
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }


}
