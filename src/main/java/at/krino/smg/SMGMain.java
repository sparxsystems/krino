package at.krino.smg;

import at.krino.AbstractComponent;
import at.krino.ds.AdpTree;

import java.util.concurrent.BlockingQueue;


/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class SMGMain extends AbstractComponent {

	public SMGMain( BlockingQueue< AdpTree > q){
            super( q );
	}

        
	public void run(){
            System.out.println( "running SMG (Semantic MegaGraph)" );
            while( ! poisonCookie.get() )   {
                AdpTree t = queue.peek();
            }
            System.out.println( "SMG (Semantic MegaGraph): stopped gracefully" );
	}


}