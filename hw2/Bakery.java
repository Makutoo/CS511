import java.util.Map;
import java.util.concurrent.*;

public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 200;
    private static final int CAPACITY = 50;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread;
    private ExecutorService executor;
    private float sales = 0;

    // TODO
    Semaphore cashiers;
    Semaphore mutexSales;
    Semaphore mutexRyeShelves;
    Semaphore mutexSourdoughShelves;
    Semaphore mutexWonderShelves;

    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        int breadLeft = availableBread.get(bread);
        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1); // Here restore bread
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        // TODO
        cashiers = new Semaphore(4);
        mutexSales = new Semaphore(1); // Sales is share variable
        mutexRyeShelves = new Semaphore(1);
        mutexSourdoughShelves = new Semaphore(1);
        mutexWonderShelves = new Semaphore(1);


        executor = Executors.newFixedThreadPool(CAPACITY);
        for(int i = 0; i < TOTAL_CUSTOMERS; i++) {
            executor.execute(new Customer(this));
        }

        executor.shutdown();
        try {
            executor.awaitTermination(1, TimeUnit.HOURS);
            System.out.printf("Total sales = %.2f\n", sales);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println(executor.toString());
    }
}
