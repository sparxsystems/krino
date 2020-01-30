/*
 * Copyright Sparx Systems Central Europe GmbH, 2020.
 * All rights reserved.
 * krino is a trademark of Sparx Systems Central Europe, Austria.
 *
 * This artefact and the enclosing repository are governed by the MIT License. 
 * You should have received a copy of the license together with the repository's contents. 
 * In case you didn't, you can find it here: https://opensource.org/licenses/MIT
 */

package at.krino.ds;

/**
 * This edge class acts as a proxy for the edge class of any graph database currently in use 
 * by krino. Although the original GoF Proxy design pattern works by inheritance, we can't use 
 * that mechanism here, as inheriting from an unknown class is impossible. 
 * 
 * @author Jan van Oort
 */
public abstract class Edge  {

    protected final boolean directed;
    
    
    protected Vertex t;
    
    protected Vertex h;
    
    
    public Edge( boolean d, Vertex tail, Vertex head )    {
        directed = d;
        t = tail;
        h = head;
    }
    
    
    public boolean isDirected() {
        return directed;
    }
    
    
    
    

    public Vertex getTail() {
        return t;
    }

    

    public Vertex getHead() {
        return h;
    }

    
    @Override
    public boolean equals(Object o) {
        if( null == o || ! ( o instanceof UndirectedEdge ) ) return false;
        else    {
            UndirectedEdge other = ( UndirectedEdge ) o;
            return t.equals( other.t ) && h.equals( other.h );
        }
    }

    
    @Override
    public int hashCode() {
        int hash = 11;
        hash = 59 * hash + this.t.hashCode();
        hash = 59 *  hash + this.h.hashCode();
        return hash;
    }


    public void destroy() {
        t = h = null;
    }
}
