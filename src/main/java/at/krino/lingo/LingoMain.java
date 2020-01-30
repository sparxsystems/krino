package at.krino.lingo;

import at.krino.AbstractComponent;
import at.krino.ds.AdpTree;

import java.util.concurrent.BlockingQueue;



/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class LingoMain extends AbstractComponent {

    
	public LingoMain(BlockingQueue< AdpTree > q){
            super( q );
	}

        
        
	public void run(){
            System.out.println( "running lingo (natural language parser)" );
            while( ! poisonCookie.get() )   {
                AdpTree t = queue.peek();
            }
            System.out.println( "lingo (natural language parser): stopped gracefully" );
	}


}