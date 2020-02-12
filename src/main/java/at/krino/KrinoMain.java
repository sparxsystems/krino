package at.krino;

import at.krino.ds.AdpTree;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CyclicBarrier;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class KrinoMain {

	

	public final static void main( String... args ) throws 
                ClassNotFoundException, 
                InstantiationException, 
                IllegalAccessException, 
                IllegalArgumentException, 
                InvocationTargetException   
        {
            
            KrinoConfig config = KrinoConfig.get();
            BlockingQueue< AdpTree > queue = new ArrayBlockingQueue<>( Integer.parseInt(config.get( "queue-size" )));
            Archiver a = new Archiver( queue, null );
            CyclicBarrier barrier = new CyclicBarrier( 4, a );
            a.barrier = barrier;
            Set< String > componentClasses = new HashSet<>();
            Set< KrinoComponent > components = new HashSet<>();
            
            componentClasses.add( config.get( "events") );
            componentClasses.add( config.get( "lingo") );
            componentClasses.add( config.get( "scenery") );
            componentClasses.add( config.get( "smg") );
            
            for( String className: componentClasses )   {
                Class<?> clazz = Class.forName(className);
                Constructor<?>[] c = clazz.getConstructors();
                for( Constructor cons: c )  {
                    if( cons.getParameterCount() == 2 ){
                        KrinoComponent comp = ( KrinoComponent ) cons.newInstance( queue, barrier );
                        components.add( comp );
                        Thread t = new Thread( comp );
                        t.start();
                    }
                }
            }
            Thread tHook = new Thread( new KrinoShutdownHook(components) );
            Runtime.getRuntime().addShutdownHook(tHook);
            
            try {
                Thread.sleep( 3000 );
                queue.offer( new DummyAdpTree() );
            } 
            catch (InterruptedException ex) {
                Thread.interrupted();
            }
	}

}
class KrinoShutdownHook implements Runnable  {

    private final Set< KrinoComponent > components;
    
    public KrinoShutdownHook( Set< KrinoComponent > comps ) {
        components = comps;
    }

    
    
    
    @Override
    public void run() {
        components.forEach((c) -> {
            c.poison();
        });
        try {
            Thread.sleep( 1000 );
        } 
        catch (InterruptedException ex) {
            Thread.interrupted();
        }
        System.out.println( "Krino shut down, exiting. Goodbye cruel world." );
    }
    
}


final class Archiver extends AbstractComponent     {

    
    
    CyclicBarrier barrier;
    
    Archiver( final BlockingQueue< AdpTree > q, CyclicBarrier b )   {
        super( q, b );
    }
    
    
    
    @Override
    public void run() {
        try {
            AdpTree t = queue.take();
            System.out.println( "Archiver: everybody's done with AdpTree " + t + ", archiving now");
            barrier.reset();
        } 
        catch (InterruptedException ex) {
            Thread.interrupted();
        }
    }

    
}