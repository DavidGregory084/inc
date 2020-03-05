package inc.bench;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import inc.rts.Function1;
import Test.Church;

public class ChurchTest {
    @Test
    public void zeroIsIdentity() {
        assertEquals(Church.zero("a"), "a");
    }

    @Test
    public void oneCallsFuncOnce() {
        Function1<String, String> appendA = a -> a + "a";
        assertEquals(Church.one(appendA).apply(""), "a");
    }

    @Test
    public void twoCallsFuncTwice() {
        Function1<String, String> appendA = a -> a + "a";
        assertEquals(Church.two(appendA).apply(""), "aa");
    }

    @Test
    public void plusTwoThreeCallsFuncFiveTimes() {
        Function1<String, String> appendA = a -> a + "a";
        String result = Church.five(appendA).apply("");
        assertEquals(result, "aaaaa");
    }

    @Test
    public void multTwoFiveCallsFuncTenTimes() {
        Function1<String, String> appendA = a -> a + "a";
        String result = Church.ten(appendA).apply("");
        assertEquals(result, "aaaaaaaaaa");
    }

    @Test
    public void ifThenElseReturnsFirstWhenTrue() {
        Function1<String, Function1<String, String>> tru = Church::tru;
        String result = Church.ifthenelse(tru).apply("foo").apply("bar");
        assertEquals(result, "foo");
    }

    @Test
    public void ifThenElseReturnsSecondWhenFalse() {
        Function1<String, Function1<String, String>> fals = Church::fals;
        String result = Church.ifthenelse(fals).apply("foo").apply("bar");
        assertEquals(result, "bar");
    }
}
