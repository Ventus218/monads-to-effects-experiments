import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Test extends AnyFlatSpec with Matchers:

  "A dummy test" should "always pass" in:
    true shouldBe true
