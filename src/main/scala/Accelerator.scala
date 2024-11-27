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
  val idle :: loopx :: erodeRight :: borderLeft :: borderLeftHelp :: borderTopBot :: write :: done :: Nil = Enum (8)
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val x = RegInit(0.U(8.W))
  val y = RegInit(0.U(8.W))
  val color = RegInit(0.U(8.W))
  val cols = RegInit(VecInit(Seq.fill(40)(0.U(8.W))))
  val colsize = 20.U

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := 0.U
  io.done := false.B

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := loopx
        addressReg := 0.U(16.W)
      }
    }
    is(loopx) {
      io.writeEnable := false.B
      color := 0.U
      addressReg := y*20.U + x
      io.address := addressReg

      when(y === 20.U) {
        y := 0.U
      }

      when(x === 20.U) {
        stateReg := done
      } .elsewhen (x === 0.U ) {
        stateReg := borderLeft
      } .elsewhen (y === 0.U || y === 19.U  ) {
        stateReg := erodeRight
      } .elsewhen (x === 19.U ) {
        //Border right
        stateReg := write
      } .otherwise{
        stateReg := erodeRight
        when((cols(y) & cols(y + colsize) &
          cols(colsize + y + 1.U) & cols(y - 1.U)) === 255.U) {
          color := 255.U
        }

      }


    }
    is(erodeRight) {
      io.address := addressReg + 1.U
      cols(y) := cols(y + colsize)
      cols(y + colsize) := io.dataRead
      color := io.dataRead & color
      stateReg := write
    }
    is(borderLeft) {
      io.address := addressReg
      cols(y) := io.dataRead
      stateReg := erodeRight
    }

    is(write) {
      io.address := addressReg + 400.U
      io.dataWrite := color
      io.writeEnable := true.B

      y := y + 1.U
      stateReg := loopx
      when(y === 19.U) {
        x := x + 1.U
      }
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }


}
