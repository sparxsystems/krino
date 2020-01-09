package at.krino.graph;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author jan
 */
public class GraphDBLayerTest {
    
    public GraphDBLayerTest() {
    }

    /**
     * Test of run method, of class GraphDBLayer.
     */
    @Test
    public void queueingBehaviour() throws InterruptedException {
        System.out.println("run");
        GraphDBLayer instance = new GraphDBLayer();
        new Thread( instance ).start();
        GraphDBQuery q = new TestQuery();
        
        GraphDBLayer.graphDBQueue.put(q);
        //busy wait for result
        while( true )   {
            if( q.getResult() != null ) break;
        }
        
        TestResult r = ( TestResult ) q.getResult();
        assertNotNull(r);
        assertTrue( r.payLoad.contains( "foobar") );
    }
    
}


class TestQuery implements GraphDBQuery< OrientDBVertex, OrientDBEdge>  {

    private TestListener listener;
    
    GraphDBQueryResult result;
    
    TestQuery() {
        listener = new TestListener();
    }
    
    
    @Override
    public GraphDBQueryResultListener getListener() {
        return listener;
    }

    @Override
    public GraphDBQueryResult getResult() {
        return result;
    }

    @Override
    public void run() {
        result = new TestResult();
        ( ( TestResult ) result ).payLoad = "result:foobar";
    }
    
}

class TestListener implements GraphDBQueryResultListener    {

    boolean calledBack = false;
    
    @Override
    public void callBack(GraphDBQueryResult res) {
        System.out.println( "---------> listener: callback received" );
        calledBack = true;
    }
    
}


class TestResult implements GraphDBQueryResult  {
    
    String payLoad;
}