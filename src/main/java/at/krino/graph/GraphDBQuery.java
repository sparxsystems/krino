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

/**
 * Implementations of this interface are meant to define various graph database queries. This interface only 
 * specifies the minimum behaviour expected of such queries, in order for the GraphDBLayer to be able 
 * to run such queries asynchronously.
 * 
 * GraphDBQueries are NOT meant to be instantiated, or formulated, by classes outside of the current package.
 * Rather, they are meant to be produced by utility classes within the current package in answer to needs 
 * formulated by other krino classes, e.g. when a krino classes needs to query a semantic construct or 
 * the linguistic domain. However, this interface is public as such "client" classes need to have a reference
 * to a query in order to obtain the query result.
 * 
 * @author Jan van Oort
 * @param <V> for implementing classes, the Vertex implementation they can work with
 * @param <E> for implementing classes, the Vertex implementation they can work with
 */
public interface GraphDBQuery<V extends Vertex, E extends Edge  >  extends Runnable {

    
    /**
     * A GraphDBQueryResultListener can be an instance of any class in krino; at this point, 
     * we don't care which class that is. 
     * 
     * @return this query's listener.
     */
    public GraphDBQueryResultListener getListener();
    
    /**
     * How the result of a query is stored in the query's implementing class, is none of our business at this point.
     * We only expect the query to respect the fundamental contract that, after its run() method has executed, 
     * some form of result is accessible through this method. 
     * @return the result of this query.
     */
    public GraphDBQueryResult getResult();
}
