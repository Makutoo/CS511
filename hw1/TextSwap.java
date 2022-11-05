import java.io.*;
import java.util.*;

/*
    Name: Ziheng Zhu
    Date: 09/13/2022
*/
public class TextSwap {

    private static String readFile(String filename, int chunkSize) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        long length = file.length();
	// The "-1" below is because of this:
	// https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline
	if ((length-1) % chunkSize!=0)
	    { throw new Exception("File size not multiple of chunk size"); };
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null){
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        // TODO: Implement me!
        Interval[] Intervals = new Interval[numChunks];
        for(int i = 0, j = 0; i < numChunks * chunkSize; i += chunkSize, j++) {
            Intervals[j] = new Interval(i, i + chunkSize - 1);
        }
        return Intervals;
    }

    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        // System.out.println(labels);
        return labels;
    }

    private static char[] runSwapper(String content, int chunkSize, int numChunks) {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);
        // TODO: Order the intervals properly, then run the Swapper instances.
        intervals = orderIntervals(intervals, labels);
        char[] buff = new char[content.length()];
        int offset = 0;
        List<Thread> threadPool = new ArrayList<>();
        for(int i = 0; i < numChunks; i++) {
            Swapper swapper = new Swapper(intervals[i], content, buff, offset);
            Thread t = new Thread(swapper);
            threadPool.add(t);
            offset += chunkSize;
        }
        for(Thread t : threadPool) t.start();
        for(Thread t : threadPool) {
            try {
                t.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        return buff;
    }

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

    private static Interval[] orderIntervals(Interval[] intervals, List<Character> labels) {
        Interval[] orderedIntervals = new Interval[intervals.length];
        for(int i = 0; i < intervals.length; i++) {
            orderedIntervals[i] = intervals[labels.get(i) - 'a'];
        }
        return orderedIntervals;
    }

     public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);
        try {
            contents = readFile(args[1],chunkSize);
            writeToFile(contents, chunkSize, contents.length() / chunkSize);
        } catch (Exception e) {
            System.out.println("Error with IO.");
            return;
        }
    }
}
