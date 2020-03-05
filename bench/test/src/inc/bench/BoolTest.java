package inc.bench;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import Test.Data.Bool;

public class BoolTest {
    @Test
    public void canCallTrueConstructor() {
        assertNotNull(Bool.True());
    }

    @Test
    public void canCallFalseConstructor() {
        assertNotNull(Bool.False());
    }
}
