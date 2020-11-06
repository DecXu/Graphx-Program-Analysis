import java.util.HashMap;

public class TestJNI
{
    //public static native String transfer(String msg, String stmt, int[] singletons);
    public static native void transfer(String stmt, int[] singletons, HashMap<Long, EdgeArray> test);
    public static native void transfer(String stmt, HashMap<Long, EdgeArray> test);

    /*static
    {
        System.loadLibrary("TestJNI");
    }*/
}
