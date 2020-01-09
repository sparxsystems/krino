/*
 * Copyright Sparx Systems Central Europe GmbH, 2020.
 * All rights reserved.
 * krino is a trademark of Sparx Systems Central Europe, Austria.
 *
 * This artefact and the enclosing repository are governed by the MIT License. 
 * You should have received a copy of the license together with the repository's contents. 
 * In case you didn't, you can find it here: https://opensource.org/licenses/MIT
 */

package at.krino.graph;


import java.util.concurrent.ArrayBlockingQueue;



/**
 * This class provides an isolation layer between krino classes and the graph db/graph db API currently in use. 
 * It is almost completely ignorant about internal krino logic, and internal krino logic knows only
 * about the fact that requests/queries are to be dumped in the queue periodically polled by an instance 
 * of this class. It is also this class that is responsible for performing queries to the graph db or graph db API
 * currently in use, and somehow notifying the query's originator of the result, if necessary and if any.
 * 
 * @author jan
 */
public class GraphDBLayer implements Runnable {

    
    
    public final static ArrayBlockingQueue< GraphDBQuery > graphDBQueue = new ArrayBlockingQueue<>( 1000 );
    
    
    @Override
    public void run() {
        
        while( true )   {
            try {
                GraphDBQuery query = graphDBQueue.take();
                //TODO: implement local thread pool, so we don't have to pay the fine for creating, then destroying
                //a Thread
                new Thread( query ).start();
                query.getListener().callBack( query.getResult() );
            } 
            catch (InterruptedException ex) {
                Thread.interrupted();
            }
        }
    }

}
