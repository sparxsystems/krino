using Krino.Vertical.Utils.Collections;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Graphs
{
    public class DirectedGraph<V, E> : IDirectedGraph<V, E>, IReadOnlyCollection<V>
    {
        private HashSet<V> myVertices;
        private DoubleKeyMultiDictionary<V, V, DirectedEdge<V, E>> myEdges;
        private DirectedEdgeEqualityComparer<V, E> myEdgeComparer;
        private IEqualityComparer<V> myVertexComparer;


        public DirectedGraph(IEqualityComparer<V> vertexEqualityComparer = null, IEqualityComparer<E> edgeEqualityComparer = null)
        {
            myVertexComparer = vertexEqualityComparer ?? EqualityComparer<V>.Default;
            myEdgeComparer = new DirectedEdgeEqualityComparer<V, E>(myVertexComparer, edgeEqualityComparer ?? EqualityComparer<E>.Default);
            myVertices = new HashSet<V>(myVertexComparer);
            myEdges = new DoubleKeyMultiDictionary<V, V, DirectedEdge<V, E>>(myVertexComparer, myVertexComparer, myEdgeComparer);
        }

        public IEnumerable<DirectedEdge<V, E>> Edges => myEdges.Select(x => x.Item3);

        public int Count => myVertices.Count;


        public void AddVertex(V vertex) => myVertices.Add(vertex);


        public bool RemoveVertex(V vertex) => myVertices.Remove(vertex);


        public bool ContainsVertex(V vertex) => myVertices.Contains(vertex);


        public void AddEdge(V from, V to, E edgeValue) => AddEdge(new DirectedEdge<V, E>(from, to) { Value = edgeValue });

        public void AddEdge(DirectedEdge<V, E> edge) => myEdges.Add(edge.From, edge.To, edge);

        public bool RemoveEdges(IEnumerable<DirectedEdge<V, E>> edgesToRemove)
        {
            HashSet<DirectedEdge<V, E>> edgesToRemoveSet = edgesToRemove.ToHashSet(myEdgeComparer);
            IEnumerable<Tuple<V, V>> fromTos = edgesToRemove.Select(x => Tuple.Create(x.From, x.To));
            bool result = myEdges.Remove(fromTos, (fromId, toId, edge) => edgesToRemoveSet.Contains(edge));
            return result;
        }

        public bool RemoveEdgesGoingFrom(V from, Predicate<DirectedEdge<V, E>> predicate = null)
        {
            bool result;

            if (predicate == null)
            {
                result = myEdges.RemoveForKey1(from);
            }
            else
            {
                result = myEdges.RemoveForKey1(from, (xFrom, xTo, xEdge) => predicate(xEdge));
            }

            return result;
        }

        public bool RemoveEdgesGoingTo(V to, Predicate<DirectedEdge<V, E>> predicate = null)
        {
            bool result;
            if (predicate == null)
            {
                result = myEdges.RemoveForKey2(to);
            }
            else
            {
                result = myEdges.RemoveForKey2(to, (xFrom, xTo, xEdge) => predicate(xEdge));
            }

            return result;
        }


        public IEnumerable<DirectedEdge<V, E>> GetEdgesGoingFrom(V from)
        {
            IEnumerable<DirectedEdge<V, E>> result = myEdges.GetValuesForKey1(from).Select(x => x.Value);
            return result;
        }

        public IEnumerable<DirectedEdge<V, E>> GetEdgesGoingTo(V to)
        {
            IEnumerable<DirectedEdge<V, E>> result = myEdges.GetValuesForKey2(to).Select(x => x.Value);
            return result;
        }

        public IEnumerable<DirectedEdge<V, E>> GetEdges(V from, V to)
        {
            IEnumerable<DirectedEdge<V, E>> result = GetEdgesGoingFrom(from).Where(x => myVertexComparer.Equals(x.To, to));
            return result;
        }

        public IEnumerator<V> GetEnumerator()
        {
            foreach (V vertex in myVertices)
            {
                yield return vertex;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
