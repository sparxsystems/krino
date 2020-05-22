using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Graphs
{
    /// <summary>
    /// Declares the directed graph.
    /// </summary>
    /// <typeparam name="V"></typeparam>
    /// <typeparam name="E"></typeparam>
    public interface IDirectedGraph<V, E> : IReadOnlyCollection<V>
    {
        /// <summary>
        /// Adds the vertex into the graph.
        /// </summary>
        /// <remarks>
        /// If a vertex with the same id already exists it throws ArgumentException.
        /// </remarks>
        /// <param name="vertex"></param>
        void AddVertex(V vertex);

        /// <summary>
        /// Removes the specified vertex.
        /// </summary>
        /// <param name="vertex"></param>
        /// <returns>true if the vertex was removed.</returns>
        bool RemoveVertex(V vertex);


        /// <summary>
        /// Returns true if the vertex exists in the graph.
        /// </summary>
        /// <param name="vertex"></param>
        /// <returns></returns>
        bool ContainsVertex(V vertex);



        /// <summary>
        /// Adds an edge into the graph.
        /// </summary>
        /// <remarks>
        /// There can be multiple same edges in the graph.
        /// </remarks>
        /// <param name="from"></param>
        /// <param name="to"></param>
        /// <param name="edgeValue"></param>
        void AddEdge(V from, V to, E edgeValue);

        /// <summary>
        /// Adds an edge into the graph.
        /// </summary>
        /// <remarks>
        /// There can be multiple same edges in the graph.
        /// </remarks>
        /// <param name="edge"></param>
        void AddEdge(DirectedEdge<V, E> edge);

        /// <summary>
        /// Removes specified edges.
        /// </summary>
        /// <param name="edgesToRemove"></param>
        bool RemoveEdges(IEnumerable<DirectedEdge<V, E>> edgesToRemove);

        /// <summary>
        /// Removes edges going from 'from' and matching the predicate.
        /// </summary>
        /// <param name="from"></param>
        /// <param name="predicate"></param>
        /// <returns></returns>
        bool RemoveEdgesGoingFrom(V from, Predicate<DirectedEdge<V, E>> predicate = null);

        /// <summary>
        /// Removes edges going to 'to' and matching the predicate.
        /// </summary>
        /// <param name="to"></param>
        /// <param name="predicate"></param>
        /// <returns></returns>
        bool RemoveEdgesGoingTo(V to, Predicate<DirectedEdge<V, E>> predicate = null);

        /// <summary>
        /// Returns all edges.
        /// </summary>
        /// <returns></returns>
        IEnumerable<DirectedEdge<V, E>> Edges { get; }

        /// <summary>
        /// Returns all edges going from 'from'.
        /// </summary>
        /// <param name="from"></param>
        /// <returns></returns>
        IEnumerable<DirectedEdge<V, E>> GetEdgesGoingFrom(V from);

        /// <summary>
        /// Returns all edges going to toId.
        /// </summary>
        /// <param name="to"></param>
        /// <returns></returns>
        IEnumerable<DirectedEdge<V, E>> GetEdgesGoingTo(V to);

        /// <summary>
        /// Returns all edges going from 'from' to toId.
        /// </summary>
        /// <param name="from"></param>
        /// <param name="to"></param>
        /// <returns></returns>
        IEnumerable<DirectedEdge<V, E>> GetEdges(V from, V to);
    }
}
