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
 * Any class in krino can implement this interface, in order to be able to interact with the graph database 
 * abstraction layer. Implementing this interface allows a class of being notified of query results through 
 * the callback method. Implementing classes are completely free in dealing with the GraphDBQueryResult they 
 * are given.
 * 
 * @author jan
 */
public interface GraphDBQueryResultListener {

    
    
    public void callBack( GraphDBQueryResult res );
}
