package at.krino.events;

import at.krino.AbstractComponent;
import at.krino.ds.AdpTree;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;
import java.util.logging.Level;
import java.util.logging.Logger;



/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class EventsMain extends AbstractComponent {

    public final static int ZERO = 65536;
    
	public EventsMain( BlockingQueue< AdpTree > q, CyclicBarrier b ) {
            super( q, b );
	}

        
        @Override
	public void run(){
            System.out.println( "running event processing (Truth Maintenance System)");
            while( ! poisonCookie.get() )   {
                AdpTree t = queue.peek();
                if( t != null ) {
                    System.out.println( "event processing (Truth Maintenance System) is consuming an AdpTree. Slurp, slurp...");
                }
                else continue;
                try {
                    synch.await();
                } 
                catch (InterruptedException ex) {
                    Thread.interrupted();
                } 
                catch (BrokenBarrierException ex) {
                    return;
                }
            }
            System.out.println( "event processing (Truth Maintenance System): stopped gracefully" );
	}


}