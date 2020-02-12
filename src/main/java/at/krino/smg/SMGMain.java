package at.krino.smg;

import at.krino.AbstractComponent;
import at.krino.ds.AdpTree;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;


/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class SMGMain extends AbstractComponent {

	public SMGMain( BlockingQueue< AdpTree > q, CyclicBarrier b){
            super( q, b );
	}

        
	public void run(){
            System.out.println( "running SMG (Semantic MegaGraph)" );
            while( ! poisonCookie.get() )   {
                AdpTree t = queue.peek();
                if( t != null) {
                    System.out.println( "SMG (Semantic MegaGraph) is consuming an AdpTree. Crunch, crunch...");
                }
                else    continue;
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
            System.out.println( "SMG (Semantic MegaGraph): stopped gracefully" );
	}


}