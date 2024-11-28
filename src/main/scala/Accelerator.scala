import chisel3._
import chisel3.util._

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
  val idle :: write :: getColor :: borderLeft :: writeMissedBlack :: finish :: bottom :: done :: Nil = Enum (8)
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
  val skipCount = RegInit(2.U(16.W))
  val mNum = RegInit(3.U(2.W))

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
      addressRegNext := y*20.U + x

      when (x === 20.U) {
        stateReg := done
      }
      y := y + 3.U

    }
    is(writeMissedBlack) {
      io.address := addressReg + 400.U  - (20.U + 20.U*mNum)
      io.dataWrite := 0.U
      io.writeEnable := true.B
      when (mNum === 3.U) {
        io.address := addressReg + 400.U
        io.dataWrite := color
        mNum := 0.U
        stateReg := writeMissedBlack
      } .elsewhen(mNum === 0.U) {
        mNum := 1.U
        stateReg := writeMissedBlack
        y := y + 3.U
        when(y >= 18.U) {
          x := x + 1.U
          y := 0.U
        }
      }.otherwise {
        mNum := 3.U
        stateReg := getColor
        addressReg := y*20.U + x
      }


      io.dataWrite := 0.U

    }

    is(getColor) {
      /*
      io.address := addressReg
      cols(y + colsize) := io.dataRead
      when(io.dataRead === 255.U) {
        lastWhite := 1.U
        colorNext := io.dataRead
        when(lastWhite === 0.U) {
        }
      } .otherwise {
        color := io.dataRead
        stateReg := writeMissedBlack
      }*/
      when (y === 3.U) {
        addressReg := y*20.U + x
      }
      when (y === 0.U) {
        stateReg := write
        color := 0.U
      } .otherwise {
        stateReg := writeMissedBlack
        color := 0.U
      }
      when(x === 20.U) {
        x := 0.U
        y := 19.U
        stateReg := bottom
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
