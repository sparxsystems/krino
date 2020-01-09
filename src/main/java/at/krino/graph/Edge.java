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
 * General behavioural contract for graph edges, as they are used by the krino graph abstraction layer. 
 * Implementing classes may be database- or driver-specific. This interface is not visible outside of the 
 * current package, as coupling between the graph database currently in use and the remainder of krino code
 * is to be avoided.
 * 
 * @author jan
 */
interface Edge {

    
    /**
     * @return true if this edge is directed, false in all other cases. Please note: if this edge is undirected, 
     * then the getTail() and getHead() methods are <b>still</b> required to contain two vertices u, v such that
     * u.equals(v) evaluates to false. Implementing classes are free to decide, in this undirected case, which 
     * vertex is "head" and which one is "tail". 
     */
    boolean isDirected();
    
    /**
     * @param <V> the Vertex implementation this class can work with.
     * @return in the directed case, return the vertex for which this edge is an outgoing edge; in the undirected case, 
     * this interface does not specify which vertex should be returned, only that the returned vertex v should be 
     * non-equal to the vertex returned by getHead().
     */
    <V extends Vertex> V getTail();
    
    /**
     * @param <V> the Vertex implementation this class can work with.
     * @return in the directed case, return the vertex for which this edge is an incoming edge; in the undirected case, 
     * this interface does not specify which vertex should be returned, only that the returned vertex v should be 
     * non-equal to the vertex returned by getTail().
     */
    <V extends Vertex> V getHead();
    
    
    /**
     * Accessor method for this Edge's <b>linguistic identifier</b>. A linguistic identifier is the identifier 
     * of some linguistic construct, e.g. a phrase or a discourse, within which this vertex represents something 
     * linguistically meaningful. The meaning is specified in a linguistic domain model, and be e.g. syntactic, 
     * semantic, or grammatical. 
     * 
     * @return a 64-bit long integer.
     */
    long getIDs();    
}
