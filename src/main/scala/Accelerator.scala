import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))


  })


  //State enum and register
  val idle :: loopx :: loopy :: erode :: erodeRight :: erodeLeft :: erodeDown :: erodeUp :: write :: done :: Nil = Enum(10)
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val x = RegInit(0.U(8.W))
  val y = RegInit(0.U(8.W))
  val color = RegInit(0.U(8.W))
  val lastY = RegInit(0.U(8.W))
  val prev = RegInit(VecInit(Seq.fill(20)(0.U(8.W))))
  val noOfWrites = RegInit(0.U(8.W))
  val cellLength = RegInit(0.U(8.W))
  val skip= RegInit(0.U(1.W))
  val currCell= RegInit(0.U(8.W))
  val currAddress = RegInit(0.U(16.W))
  val currColor = RegInit(0.U(8.W))

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
      when((y > 1.U)) {
        io.address := currAddress
        io.dataWrite := currColor
        io.writeEnable := true.B
      }

      when(y === 20.U) {
        y := 0.U
      }
      when(x === 20.U) {
        stateReg := done
      }.elsewhen(x === 0.U | y === 0.U | x === 19.U | y === 19.U) {
        currCell := 0.U
        stateReg := write
      }.otherwise {
        stateReg := erode
      }

      addressReg := y * 20.U + x
      color := 0.U


    }

    is(erode) {
      io.address := addressReg
      when(io.dataRead === 255.U) {
        currCell := 255.U
        stateReg := erodeLeft
      }.otherwise {
        currCell := 0.U
        stateReg := write
      }
    }
    is(erodeLeft) {
      io.address := addressReg - 1.U
      stateReg := Mux(io.dataRead === 255.U, erodeRight, write)
    }
      is(erodeRight) {
        io.address := addressReg + 1.U
        stateReg := Mux(io.dataRead === 255.U, erodeDown, write)
      }
      is(erodeDown) {
        io.address := addressReg + 20.U
        stateReg := Mux(io.dataRead === 255.U, erodeUp, write)
      }
      is(erodeUp) {
        io.address := addressReg - 20.U
        color := io.dataRead
        stateReg := write
      }

      is(write) {
          when(skip === 1.U) {
            io.address := addressReg + 400.U - 20.U
            io.dataWrite := 0.U
            io.writeEnable := true.B
          }

          currAddress := addressReg + 400.U
          currColor := color
          when(currCell === 0.U && y < 18.U) {
            y := y + 2.U
            skip := 1.U
            stateReg := loopx
            when(y === 19.U) {
              x := x + 1.U
            }
          }.otherwise {
            y := y + 1.U
            skip := 0.U
            stateReg := loopx
            when(y === 19.U) {
              x := x + 1.U
            }
          }
        }


      is(done) {
        io.done := true.B
        stateReg := done
      }
    }

}

