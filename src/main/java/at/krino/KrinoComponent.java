package at.krino;

import java.lang.Runnable;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CyclicBarrier;
import at.krino.ds.AdpTree;


/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public interface KrinoComponent extends Runnable {

    
    
        public void poison();
    
        @Override
	public void run();

}