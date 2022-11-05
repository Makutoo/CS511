import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        // TODO
        this.bakery = bakery;
        rnd = new Random();
        shoppingCart = new ArrayList<>();
        this.shopTime = rnd.nextInt(500);
        this.checkoutTime = rnd.nextInt(1000);
        fillShoppingCart();
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO

        // Take bread
        for(BreadType bread : shoppingCart) {
            if(bread == BreadType.RYE) {
                try {
                    bakery.mutexRyeShelves.acquire();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                try {
                    Thread.sleep(shopTime);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                this.bakery.takeBread(bread);
                bakery.mutexRyeShelves.release();
            } else if(bread == BreadType.SOURDOUGH) {
                try {
                    bakery.mutexSourdoughShelves.acquire();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                try {
                    Thread.sleep(shopTime);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                this.bakery.takeBread(bread);
                bakery.mutexSourdoughShelves.release();
            } else {
                try {
                    bakery.mutexWonderShelves.acquire();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                try {
                    Thread.sleep(shopTime);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                this.bakery.takeBread(bread);
                bakery.mutexWonderShelves.release();
            }

        }

        // Pay bread
        try {
            bakery.cashiers.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        try {
            bakery.mutexSales.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        try {
            Thread.sleep(checkoutTime);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        this.bakery.addSales(getItemsValue());
        bakery.mutexSales.release();

        bakery.cashiers.release();
        System.out.println(this.toString());
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}