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

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A hyperedge in a hypergraph. All operations of the implemented Set interface are delegated to a 
 * contained, existing Set implementation. 
 * 
 * TODO: this class is not thread-safe w/ regards to operations on the contained vertex set, 
 * which is a HashSet, which itself is not thread-safe. There may be a need for thread-safety. 
 * 
 * @author Jan van Oort
 */
public class HyperEdgeImpl extends Vertex implements HyperEdge {

    
    
    private final Set< Vertex > vertices = new HashSet<>();
    
    
    
    @Override
    public int size() {
        return vertices.size();
    }

    
    @Override
    public boolean isEmpty() {
        return vertices.isEmpty();
    }

    
    @Override
    public boolean contains(Object o) {
        return vertices.contains(o);
    }

    
    @Override
    public Iterator<Vertex> iterator() {
        return vertices.iterator();
    }

    
    @Override
    public Object[] toArray() {
        return vertices.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return vertices.toArray(a);
    }

    @Override
    public boolean add(Vertex e) {
        return vertices.add(e);
    }

    
    @Override
    public boolean remove(Object o) {
        return vertices.remove(o);
    }

    
    @Override
    public boolean containsAll(Collection<?> c) {
        return vertices.containsAll(c);
    }

    
    @Override
    public boolean addAll(Collection<? extends Vertex> c) {
        return vertices.addAll(c);
    }

    
    @Override
    public boolean retainAll(Collection<?> c) {
        return vertices.retainAll(c);
    }

    
    @Override
    public boolean removeAll(Collection<?> c) {
        return vertices.removeAll(c);
    }

    
    @Override
    public void clear() {
        vertices.clear();
    }

    
    /**
     * Calculates the set-theoretic intersection between this hyperedge and one given as an parameter, 
     * and returns the result in a newly instantiated Set. 
     * @param h
     * @return 
     */
    @Override
    public Set<Vertex> intersection(HyperEdge h) {
        Set< Vertex > intersection = new HashSet<>( vertices );
        intersection.retainAll( h.getVertices() );
        return intersection;
    }

    
    @Override
    public Set<Vertex> getVertices() {
        return vertices;
    }

    
    @Override
    public void destroy()   {
        super.destroy();
        vertices.clear();
    }
    
}
