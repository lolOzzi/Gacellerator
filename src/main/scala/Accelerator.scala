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
  val idle :: loopx :: loopy :: erode :: erodeRight :: erodeLeft :: erodeDown :: erodeUp :: write :: done :: Nil = Enum (10)
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val x = RegInit(0.U(8.W))
  val y = RegInit(0.U(8.W))
  val color = RegInit(0.U(8.W))
  val lastY = RegInit(0.U(8.W))
  val prev = RegInit(VecInit(Seq.fill(20)(0.U(8.W))))

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

      when(y === 20.U) {
        y := 0.U
      }
      when(x === 20.U) {
        stateReg := done
      } .elsewhen (x === 0.U | y === 0.U | x === 19.U | y === 19.U ) {
        stateReg := write
      } .otherwise{
        stateReg := erode
      }

      addressReg := y*20.U + x
      color := 0.U

    }

    is(erode) {
      io.address := addressReg
      when(io.dataRead === 255.U && lastY === 255.U && prev(y) === 255.U) {
        stateReg := erodeRight
      } .otherwise {
        stateReg := write
      }
      lastY := io.dataRead
      prev(y) := io.dataRead

    }
    is(erodeRight) {
      io.address := addressReg + 1.U
      stateReg := Mux(io.dataRead === 255.U, erodeDown, write)
    }
    is(erodeDown) {
      io.address := addressReg + 20.U
      when(io.dataRead === 255.U & lastY === 255.U) {
        color := 255.U
      } .otherwise {
        color := 0.U
      }
      stateReg := write
    }
    is(erodeUp) {
      io.address := addressReg - 20.U
      color := io.dataRead
      stateReg := write
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
