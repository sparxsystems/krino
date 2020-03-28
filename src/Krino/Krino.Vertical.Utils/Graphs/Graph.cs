using Krino.Vertical.Utils.Collections;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Graphs
{
    public class Graph<V, E> : IGraph<V, E>
    {
        private Dictionary<string, Vertex<V>> myVertices = new Dictionary<string, Vertex<V>>();
        private DoubleKeyMultiDictionary<string, string, Edge<E>> myEdges = new DoubleKeyMultiDictionary<string, string, Edge<E>>();
        private EdgeEqualityComparer<E> myEdgeComparer;


        public Graph(IEqualityComparer<E> edgeValueEqualityComparer = null)
        {
            myEdgeComparer = new EdgeEqualityComparer<E>(edgeValueEqualityComparer);
        }

        public IEnumerable<Edge<E>> Edges => myEdges.Select(x => x.Item3);

        public int Count => myVertices.Count;


        public void AddVertex(Vertex<V> vertex)
        {
            myVertices.Add(vertex.Id, vertex);
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



        public void AddEdge(string fromId, string toId, string type, E edgeValue)
        {
            Edge<E> edge = new Edge<E>() { From = fromId, To = toId, Type = type, Value = edgeValue };
            myEdges.Add(fromId, toId, edge);
        }

        public bool RemoveEdges(IEnumerable<Edge<E>> edgesToRemove)
        {
            HashSet<Edge<E>> edgesToRemoveSet = edgesToRemove.ToHashSet(myEdgeComparer);
            IEnumerable<Tuple<string, string>> fromTos = edgesToRemove.Select(x => Tuple.Create(x.From, x.To));
            bool result = myEdges.Remove(fromTos, (fromId, toId, edge) => edgesToRemoveSet.Contains(edge));
            return result;
        }

        public bool RemoveEdgesGoingFrom(string fromId, Predicate<Edge<E>> predicate = null)
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

        public bool RemoveEdgesGoingTo(string toId, Predicate<Edge<E>> predicate = null)
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


        public IEnumerable<Edge<E>> GetEdgesGoingFrom(string fromId)
        {
            IEnumerable<Edge<E>> result = myEdges.GetValuesForKey1(fromId).Select(x => x.Value);
            return result;
        }

        public IEnumerable<Edge<E>> GetEdgesGoingTo(string toId)
        {
            IEnumerable<Edge<E>> result = myEdges.GetValuesForKey2(toId).Select(x => x.Value);
            return result;
        }

        public IEnumerable<Edge<E>> GetEdges(string fromId, string toId)
        {
            IEnumerable<Edge<E>> result = GetEdgesGoingFrom(fromId).Where(x => x.To == toId);
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
