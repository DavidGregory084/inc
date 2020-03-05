package inc.bench;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import Test.Const;

public class ConstTest {
    @Test
    public void constReturnsFirstArgument() {
        assertEquals(Const.constant("foo", "bar"), "foo");
    }

    @Test
    public void constIsPolymorphic() {
        assertEquals(Const.app, "a");
    }
}
