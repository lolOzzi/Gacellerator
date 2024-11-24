import chisel3._
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester

import java.util

class ErodeTester(dut: Accelerator) extends PeekPokeTester(dut) {
}

object ErodeTester {
  def main(args: Array[String]): Unit = {
    println("Running the Hello tester")
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on",
        "--target-dir", "generated",
        "--top-name", "Hello"),
      () => new Accelerator()) {
      c => new ErodeTester(c)
    }
  }
}
