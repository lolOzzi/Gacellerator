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
  val idle :: write :: erodeRight :: borderLeft :: finish :: done :: Nil = Enum (6)
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val x = RegInit(0.U(8.W))
  val y = RegInit(0.U(8.W))
  val color = RegInit(0.U(8.W))
  val colorNext = RegInit(0.U(8.W))
  val cols = RegInit(VecInit(Seq.fill(40)(0.U(8.W))))
  val colsize = 20.U
  val init = RegInit(0.U(1.W))
  val addressRegNext = RegInit(0.U(16.W))

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


      colorNext := 0.U
      addressRegNext := y*20.U + x

      when(x === 20.U) {
        stateReg := finish
      } .elsewhen (x === 0.U ) {
        stateReg := borderLeft
      }.elsewhen (x === 19.U ) {
        y := y + 1.U
        when(y === 19.U) {
          x := 20.U
        }
        addressReg := addressRegNext
        colorNext := 0.U
        stateReg := write
      } .elsewhen (y === 0.U || y === 19.U  ) {
        stateReg := erodeRight
      } .otherwise{
        stateReg := erodeRight
        when((cols(y) & cols(y + colsize) &
          cols(colsize + y + 1.U) & cols(y - 1.U)) === 255.U) {
          colorNext := 255.U
        }
      }
    }
    is(erodeRight) {
      io.address := addressRegNext + 1.U
      addressReg := addressRegNext
      cols(y) := cols(y + colsize)
      cols(y + colsize) := io.dataRead
      color := io.dataRead & colorNext
      stateReg := write
      y := y + 1.U
      when(y === 19.U) {
        x := x + 1.U
        y := 0.U
      }
    }
    is(borderLeft) {
      colorNext := 0.U;
      io.address := addressRegNext
      cols(y + colsize) := io.dataRead
      stateReg := erodeRight
    }

    is(finish){
      io.address := 799.U
      io.writeEnable := true.B
      io.dataWrite := 0.U
      stateReg := done
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }


}
