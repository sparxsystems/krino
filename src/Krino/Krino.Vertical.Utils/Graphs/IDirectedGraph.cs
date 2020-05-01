using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Graphs
{
    public interface IDirectedGraph<V, E> : IReadOnlyCollection<Vertex<V>>
    {
        /// <summary>
        /// Adds the vertex into the graph.
        /// </summary>
        /// <remarks>
        /// If a vertex with the same id already exists it throws ArgumentException.
        /// </remarks>
        /// <param name="vertex"></param>
        void AddVertex(Vertex<V> vertex);

        /// <summary>
        /// Adds the vertex into the graph.
        /// </summary>
        /// <param name="id"></param>
        /// <param name="vertexValue"></param>
        void AddVertex(string id, V vertexValue);

        /// <summary>
        /// Removes the vertex with the specified id.
        /// </summary>
        /// <remarks>
        /// Because edges can be added to the graph independently from vertices (E.g. first you can add edges and then vertices)
        /// this will remove only the specified vertex and NOT related edges.
        /// </remarks>
        /// <param name="id"></param>
        /// <returns>true if the vertex was removed.</returns>
        bool RemoveVertex(string id);

        /// <summary>
        /// Returns the vertex with the specified id. It returns null if the vertex does not exist.
        /// </summary>
        /// <param name="id"></param>
        /// <returns></returns>
        Vertex<V> TryGetVertex(string id);

        /// <summary>
        /// Gets or sets-add the vertex with the specified id.
        /// </summary>
        /// <remarks>
        /// It throws KeyNotFoundException if 'get' is called and the vertex is not found.
        /// </remarks>
        /// <param name="id"></param>
        /// <returns></returns>
        Vertex<V> this[string id] { get; set; }

        /// <summary>
        /// Returns true if the vertex with the specified id exists.
        /// </summary>
        /// <param name="id"></param>
        /// <returns></returns>
        bool ContainsVertex(string id);



        /// <summary>
        /// Adds an edge into the graph.
        /// </summary>
        /// <remarks>
        /// There can be multiple same edges in the graph.
        /// </remarks>
        /// <param name="fromId"></param>
        /// <param name="toId"></param>
        /// <param name="edgeValue"></param>
        void AddEdge(string fromId, string toId, E edgeValue);

        /// <summary>
        /// Removes specified edges.
        /// </summary>
        /// <param name="edgesToRemove"></param>
        bool RemoveEdges(IEnumerable<DirectedEdge<E>> edgesToRemove);

        /// <summary>
        /// Removes edges going fromId and matching the predicate.
        /// </summary>
        /// <param name="fromId"></param>
        /// <param name="predicate"></param>
        /// <returns></returns>
        bool RemoveEdgesGoingFrom(string fromId, Predicate<DirectedEdge<E>> predicate = null);

        /// <summary>
        /// Removes edges going to toId and matching the predicate.
        /// </summary>
        /// <param name="toId"></param>
        /// <param name="predicate"></param>
        /// <returns></returns>
        bool RemoveEdgesGoingTo(string toId, Predicate<DirectedEdge<E>> predicate = null);

        /// <summary>
        /// Returns all edges.
        /// </summary>
        /// <returns></returns>
        IEnumerable<DirectedEdge<E>> Edges { get; }

        /// <summary>
        /// Returns all edges going from fromId.
        /// </summary>
        /// <param name="fromId"></param>
        /// <returns></returns>
        IEnumerable<DirectedEdge<E>> GetEdgesGoingFrom(string fromId);

        /// <summary>
        /// Returns all edges going to toId.
        /// </summary>
        /// <param name="toId"></param>
        /// <returns></returns>
        IEnumerable<DirectedEdge<E>> GetEdgesGoingTo(string toId);

        /// <summary>
        /// Returns all edges going from fromId to toId.
        /// </summary>
        /// <param name="fromId"></param>
        /// <param name="toId"></param>
        /// <returns></returns>
        IEnumerable<DirectedEdge<E>> GetEdges(string fromId, string toId);
    }
}
