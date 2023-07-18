
import java.util.Objects;

public class Test2 {
	public static final int FOO = getFoo();
	public static final byte BAR = (byte) 0;
	public boolean baz = true;
	public long foo = 0L;
	public double bar = 0.0;
	public short k = 0;

	public static int getFoo() {
		return 10;
	}
	public static void main(String[] args) {
		System.out.println(FOO);

		for (int i = 1; i < FOO; i *= 2) {
			System.out.println(i);
		}

		String s0 = "1" + FOO + "2" + "3";
		System.out.println(s0.hashCode() + " " + Objects.hashCode(s0));

		String s1 = s0.intern();
		System.out.println(s1.hashCode() + " " + Objects.hashCode(s1));

		String s2 = new String(new char[]{'1', '1', '0', '2', '3'});
		System.out.println(s2.hashCode() + " " + Objects.hashCode(s2));
	}
}