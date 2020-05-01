using Krino.Vertical.Utils.Graphs;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils_Tests.Graphs
{
    [TestFixture]
    public class DirectedGraphTest
    {
        [Test]
        public void AddVertex()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Assert.AreEqual(2, graph.Count);
            Assert.AreEqual("id1", graph["id1"].Id);
            Assert.AreEqual("A", graph["id1"].Value);
            Assert.AreEqual("id2", graph["id2"].Id);
            Assert.AreEqual("B", graph["id2"].Value);
        }

        [Test]
        public void AddVertex_AlreadyExists()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });

            Assert.Throws<ArgumentException>(() => graph.AddVertex(new Vertex<string>("id1") { Value = "B" }));
        }

        [Test]
        public void RemoveVertex()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Assert.IsTrue(graph.RemoveVertex("id1"));

            Assert.AreEqual(1, graph.Count);
            Assert.AreEqual("id2", graph["id2"].Id);
            Assert.AreEqual("B", graph["id2"].Value);

            Assert.IsFalse(graph.RemoveVertex("bla"));
        }

        [Test]
        public void TryGetVertex()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Vertex<string> vertex = graph.TryGetVertex("id2");

            Assert.IsNotNull(vertex);
            Assert.AreEqual("id2", vertex.Id);
            Assert.AreEqual("B", vertex.Value);

            Assert.IsNull(graph.TryGetVertex("bla"));
        }

        [Test]
        public void GetVertex_Operator()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Vertex<string> vertex = graph["id2"];

            Assert.IsNotNull(vertex);
            Assert.AreEqual("id2", vertex.Id);
            Assert.AreEqual("B", vertex.Value);

            Assert.Throws<KeyNotFoundException>(() => vertex = graph["bla"]);
        }

        [Test]
        public void ContainsVertex()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Assert.IsTrue(graph.ContainsVertex("id2"));
            Assert.IsFalse(graph.ContainsVertex("bla"));
        }

        [Test]
        public void Count()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Assert.AreEqual(2, graph.Count);
        }

        [Test]
        public void Vertex_Enumerator()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Assert.IsTrue(graph.Any(x => x.Id == "id1" && x.Value == "A"));
            Assert.IsTrue(graph.Any(x => x.Id == "id2" && x.Value == "B"));
        }


        [Test]
        public void AddEdge()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddEdge("id1", "id2", 10);
            graph.AddEdge("id1", "id3", 20);

            Assert.AreEqual(2, graph.Edges.Count());
        }

        [Test]
        public void RemoveEdges()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddEdge("id1", "id2", 10);
            graph.AddEdge("id1", "id3", 20);
            graph.AddEdge("id1", "id4", 30);

            DirectedEdge<int>[] toRemove = new DirectedEdge<int>[]
            {
                new DirectedEdge<int>("id1", "id2") { Value = 10 },
                new DirectedEdge<int>("id1", "id3") { Value = 20 },
            };

            Assert.IsTrue(graph.RemoveEdges(toRemove));

            Assert.AreEqual(1, graph.Edges.Count());
            Assert.AreEqual(30, graph.GetEdges("id1", "id4").First().Value);

            // Note: the edges in toRemove do not exist anymore, so it should return false.
            Assert.IsFalse(graph.RemoveEdges(toRemove));
        }

        [Test]
        public void RemoveEdgesGoingFrom()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddEdge("id1", "id2", 10);
            graph.AddEdge("id1", "id3", 20);
            graph.AddEdge("id2", "id4", 30);

            Assert.IsTrue(graph.RemoveEdgesGoingFrom("id1"));

            Assert.AreEqual(1, graph.Edges.Count());
            Assert.AreEqual(30, graph.GetEdges("id2", "id4").First().Value);

            Assert.IsFalse(graph.RemoveEdgesGoingFrom("id1"));
        }

        [Test]
        public void RemoveEdgesGoingTo()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddEdge("id1", "id2", 10);
            graph.AddEdge("id3", "id2", 20);
            graph.AddEdge("id2", "id4", 30);

            Assert.IsTrue(graph.RemoveEdgesGoingTo("id2"));

            Assert.AreEqual(1, graph.Edges.Count());
            Assert.AreEqual(30, graph.GetEdges("id2", "id4").First().Value);

            Assert.IsFalse(graph.RemoveEdgesGoingTo("id2"));
        }

        [Test]
        public void GetEdgesGoingFrom()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddEdge("id1", "id2", 10);
            graph.AddEdge("id1", "id2", 10); // same one
            graph.AddEdge("id1", "id3", 20);
            graph.AddEdge("id2", "id4", 30);

            IEnumerable<DirectedEdge<int>> edges = graph.GetEdgesGoingFrom("id1"); 

            Assert.AreEqual(3, edges.Count());
            Assert.AreEqual(2, edges.Where(x => x.From == "id1" && x.To == "id2" && x.Value == 10).Count());
            Assert.AreEqual(1, edges.Where(x => x.From == "id1" && x.To == "id3" && x.Value == 20).Count());

            Assert.AreEqual(0, graph.GetEdgesGoingFrom("bla").Count());
        }

        [Test]
        public void GetEdgesGoingTo()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddEdge("id1", "id2", 10);
            graph.AddEdge("id1", "id2", 10); // same one
            graph.AddEdge("id3", "id2", 20);
            graph.AddEdge("id2", "id4", 30);

            IEnumerable<DirectedEdge<int>> edges = graph.GetEdgesGoingTo("id2");

            Assert.AreEqual(3, edges.Count());
            Assert.AreEqual(2, edges.Where(x => x.From == "id1" && x.To == "id2" && x.Value == 10).Count());
            Assert.AreEqual(1, edges.Where(x => x.From == "id3" && x.To == "id2" && x.Value == 20).Count());

            Assert.AreEqual(0, graph.GetEdgesGoingTo("bla").Count());
        }

        [Test]
        public void GetEdges()
        {
            DirectedGraph<string, int> graph = new DirectedGraph<string, int>();
            graph.AddEdge("id1", "id2", 10);
            graph.AddEdge("id1", "id2", 10); // same one
            graph.AddEdge("id3", "id2", 20);
            graph.AddEdge("id2", "id4", 30);

            IEnumerable<DirectedEdge<int>> edges = graph.GetEdges("id1", "id2");

            Assert.AreEqual(2, edges.Count());
            Assert.AreEqual(2, edges.Where(x => x.From == "id1" && x.To == "id2" && x.Value == 10).Count());

            Assert.AreEqual(0, graph.GetEdges("bla1", "bla2").Count());
        }
    }
}
