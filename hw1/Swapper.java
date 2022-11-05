public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }


    @Override
    public void run() {
        // TODO: Implement me!
        String s = content.substring(interval.getX(), interval.getY()+1);
        int num = offset / (interval.getY() - interval.getX() + 1) + 1;
        for(int i = 0; i < s.length(); i++) {
            buffer[i+offset] = s.charAt(i);
            System.out.println("Thread" + num + " wrote \'" + s.charAt(i) + "\' to buffer at index: " + (i + offset));
        }
    }
}