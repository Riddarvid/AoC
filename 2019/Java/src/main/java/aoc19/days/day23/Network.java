package aoc19.days.day23;

import java.util.HashMap;
import java.util.Map;

public class Network {
    private Map<Long, NetworkCommunicator> addressMap;
    private NAT nat;

    public Network(int numberOfNodes, long[] memory) {
        addressMap = new HashMap<>();
        for (long i = 0; i < numberOfNodes; i++) {
            NetworkCommunicator cm = new NetworkCommunicator(memory, i);
            addressMap.put(i, cm);
        }
        nat = new NAT(addressMap.get(0L), this);
        NetworkCommunicator.setAddressMap(addressMap);
        NetworkCommunicator.setNat(nat);
    }

    public void run() {
        for (long i = 0; i < addressMap.size(); i++) {
            NetworkCommunicator cm = addressMap.get(i);
            Thread thread = new Thread(cm);
            thread.start();
        }
        Thread thread = new Thread(nat);
        thread.start();
    }

    public boolean isIdle() {
        int hashBefore = addressMap.hashCode();
        try {
            Thread.sleep(10);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        int hashAfter = addressMap.hashCode();
        return hashBefore == hashAfter;
    }
}
