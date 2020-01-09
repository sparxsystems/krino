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

import java.util.Set;

/**
 * General behavioural contract for graph edges, as they are used by the krino graph abstraction layer.  
 * Implementing classes may be database- or driver-specific. This interface is not visible outside of the 
 * current package, as coupling between the graph database currently in use and the remainder of krino code
 * is to be avoided.
 * 
 * Where reasonable or desired, this interface's accessor methods return instances of java.util.Set, as that interface's 
 * contract stipulates that a Set may contain no duplicate elements.
 * 
 * @author jan
 */
interface Vertex {
    
    /**
     * Accessor method for all of this vertex' edges. 
     * @param <E> the actual implementation of the Edge interface this method can work with. 
     * @return the set of edges of this vertex. Please note: if some of the edges attached to this vertex are 
     * directed, then the returned set may contain both directed and undirected edges. 
     */
    <E extends Edge > Set< E > getEdges();
    
    
    /**
     * Accessor method for this vertex' incoming edges.
     * @param <E> the actual implementation of the Edge interface this method can work with. 
     * @return the set of incoming edges of this vertex. The returned set will contain only directed edges, 
     * if there are such edges.
     */
    <E extends Edge> Set< E > getInEdges();
    
    /**
     * Accessor method for this vertex' outgoing edges.
     * @param <E> the actual implementation of the Edge interface this method can work with. 
     * @return the set of outgoing edges of this vertex. The returned set will contain only directed edges, 
     * if there are such edges.
     */
    <E extends Edge> Set< E > getOutEdges();
    
    
    /**
     * Accessor method for this Vertex' <b>linguistic identifiers</b>. A linguistic identifier is the identifier 
     * of some linguistic construct, e.g. a phrase or a discourse, within which this vertex represents something 
     * linguistically meaningful. The meaning is specified in a linguistic domain model, and be e.g. syntactic, 
     * semantic, or grammatical. 
     * 
     * @return an array of 64-bit long integers.
     */
    long[] getIDs();

}
