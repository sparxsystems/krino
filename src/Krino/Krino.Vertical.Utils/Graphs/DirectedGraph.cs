using Krino.Vertical.Utils.Collections;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Graphs
{
    public class DirectedGraph<V, E> : IDirectedGraph<V, E>
    {
        private Dictionary<string, Vertex<V>> myVertices = new Dictionary<string, Vertex<V>>();
        private DoubleKeyMultiDictionary<string, string, DirectedEdge<E>> myEdges = new DoubleKeyMultiDictionary<string, string, DirectedEdge<E>>();
        private DirectedEdgeEqualityComparer<E> myEdgeComparer;


        public DirectedGraph(IEqualityComparer<E> edgeValueEqualityComparer = null)
        {
            myEdgeComparer = new DirectedEdgeEqualityComparer<E>(edgeValueEqualityComparer);
        }

        public IEnumerable<DirectedEdge<E>> Edges => myEdges.Select(x => x.Item3);

        public int Count => myVertices.Count;


        public void AddVertex(Vertex<V> vertex)
        {
            myVertices.Add(vertex.Id, vertex);
        }

        public void AddVertex(string id, V vertexValue)
        {
            Vertex<V> vertex = new Vertex<V>(id) { Value = vertexValue };
            AddVertex(vertex);
        }

        public bool RemoveVertex(string id)
        {
            bool result = myVertices.Remove(id);
            return result;
        }

        public Vertex<V> TryGetVertex(string id)
        {
            myVertices.TryGetValue(id, out Vertex<V> result);
            return result;
        }

        public Vertex<V> this[string id]
        {
            get => myVertices[id];
            set => myVertices[id] = value;
        }

        public bool ContainsVertex(string id)
        {
            bool result = myVertices.ContainsKey(id);
            return result;
        }



        public void AddEdge(string fromId, string toId, E edgeValue)
        {
            DirectedEdge<E> edge = new DirectedEdge<E>(fromId, toId) { Value = edgeValue };
            myEdges.Add(fromId, toId, edge);
        }

        public bool RemoveEdges(IEnumerable<DirectedEdge<E>> edgesToRemove)
        {
            HashSet<DirectedEdge<E>> edgesToRemoveSet = edgesToRemove.ToHashSet(myEdgeComparer);
            IEnumerable<Tuple<string, string>> fromTos = edgesToRemove.Select(x => Tuple.Create(x.From, x.To));
            bool result = myEdges.Remove(fromTos, (fromId, toId, edge) => edgesToRemoveSet.Contains(edge));
            return result;
        }

        public bool RemoveEdgesGoingFrom(string fromId, Predicate<DirectedEdge<E>> predicate = null)
        {
            bool result;

            if (predicate == null)
            {
                result = myEdges.RemoveForKey1(fromId);
            }
            else
            {
                result = myEdges.RemoveForKey1(fromId, (fromId, toId, edge) => predicate(edge));
            }

            return result;
        }

        public bool RemoveEdgesGoingTo(string toId, Predicate<DirectedEdge<E>> predicate = null)
        {
            bool result;
            if (predicate == null)
            {
                result = myEdges.RemoveForKey2(toId);
            }
            else
            {
                result = myEdges.RemoveForKey2(toId, (fromId, toId, edge) => predicate(edge));
            }

            return result;
        }


        public IEnumerable<DirectedEdge<E>> GetEdgesGoingFrom(string fromId)
        {
            IEnumerable<DirectedEdge<E>> result = myEdges.GetValuesForKey1(fromId).Select(x => x.Value);
            return result;
        }

        public IEnumerable<DirectedEdge<E>> GetEdgesGoingTo(string toId)
        {
            IEnumerable<DirectedEdge<E>> result = myEdges.GetValuesForKey2(toId).Select(x => x.Value);
            return result;
        }

        public IEnumerable<DirectedEdge<E>> GetEdges(string fromId, string toId)
        {
            IEnumerable<DirectedEdge<E>> result = GetEdgesGoingFrom(fromId).Where(x => x.To == toId);
            return result;
        }

        public IEnumerator<Vertex<V>> GetEnumerator()
        {
            foreach (Vertex<V> vertex in myVertices.Values)
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
