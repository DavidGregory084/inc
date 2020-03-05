package inc.bench;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import Test.Main.Lambda;

public class LambdaTest {
    @Test
    public void lamReturns42WhenTrue() {
        assertEquals(Lambda.lam(true), 42);
    }

    @Test
    public void lamReturns41WhenFalse() {
        assertEquals(Lambda.lam(false), 41);
    }
}
