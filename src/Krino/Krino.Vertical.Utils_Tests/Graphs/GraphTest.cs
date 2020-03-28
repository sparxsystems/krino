using Krino.Vertical.Utils.Graphs;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Vertical.Utils_Tests.Graphs
{
    [TestFixture]
    public class GraphTest
    {
        [Test]
        public void AddVertex()
        {
            Graph<string, int> graph = new Graph<string, int>();
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
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });

            Assert.Throws<ArgumentException>(() => graph.AddVertex(new Vertex<string>("id1") { Value = "B" }));
        }

        [Test]
        public void RemoveVertex()
        {
            Graph<string, int> graph = new Graph<string, int>();
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
            Graph<string, int> graph = new Graph<string, int>();
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
            Graph<string, int> graph = new Graph<string, int>();
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
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Assert.IsTrue(graph.ContainsVertex("id2"));
            Assert.IsFalse(graph.ContainsVertex("bla"));
        }

        [Test]
        public void Count()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Assert.AreEqual(2, graph.Count);
        }

        [Test]
        public void Vertex_Enumerator()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddVertex(new Vertex<string>("id1") { Value = "A" });
            graph.AddVertex(new Vertex<string>("id2") { Value = "B" });

            Assert.IsTrue(graph.Any(x => x.Id == "id1" && x.Value == "A"));
            Assert.IsTrue(graph.Any(x => x.Id == "id2" && x.Value == "B"));
        }


        [Test]
        public void AddEdge()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddEdge("id1", "id2", "type1", 10);
            graph.AddEdge("id1", "id3", "type2", 20);

            Assert.AreEqual(2, graph.Edges.Count());
        }

        [Test]
        public void RemoveEdges()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddEdge("id1", "id2", "type1", 10);
            graph.AddEdge("id1", "id3", "type2", 20);
            graph.AddEdge("id1", "id4", "type2", 30);

            Edge<int>[] toRemove = new Edge<int>[]
            {
                new Edge<int>() { From = "id1", To = "id2", Type = "type1", Value = 10 },
                new Edge<int>() { From = "id1", To = "id3", Type = "type2", Value = 20 },
            };

            Assert.IsTrue(graph.RemoveEdges(toRemove));

            Assert.AreEqual(1, graph.Edges.Count());
            Assert.AreEqual("type2", graph.GetEdges("id1", "id4").First().Type);
            Assert.AreEqual(30, graph.GetEdges("id1", "id4").First().Value);

            // Note: the edges in toRemove do not exist anymore, so it should return false.
            Assert.IsFalse(graph.RemoveEdges(toRemove));
        }

        [Test]
        public void RemoveEdgesGoingFrom()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddEdge("id1", "id2", "type1", 10);
            graph.AddEdge("id1", "id3", "type2", 20);
            graph.AddEdge("id2", "id4", "type2", 30);

            Assert.IsTrue(graph.RemoveEdgesGoingFrom("id1"));

            Assert.AreEqual(1, graph.Edges.Count());
            Assert.AreEqual("type2", graph.GetEdges("id2", "id4").First().Type);
            Assert.AreEqual(30, graph.GetEdges("id2", "id4").First().Value);

            Assert.IsFalse(graph.RemoveEdgesGoingFrom("id1"));
        }

        [Test]
        public void RemoveEdgesGoingTo()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddEdge("id1", "id2", "type1", 10);
            graph.AddEdge("id3", "id2", "type2", 20);
            graph.AddEdge("id2", "id4", "type2", 30);

            Assert.IsTrue(graph.RemoveEdgesGoingTo("id2"));

            Assert.AreEqual(1, graph.Edges.Count());
            Assert.AreEqual("type2", graph.GetEdges("id2", "id4").First().Type);
            Assert.AreEqual(30, graph.GetEdges("id2", "id4").First().Value);

            Assert.IsFalse(graph.RemoveEdgesGoingTo("id2"));
        }

        [Test]
        public void GetEdgesGoingFrom()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddEdge("id1", "id2", "type1", 10);
            graph.AddEdge("id1", "id2", "type1", 10); // same one
            graph.AddEdge("id1", "id3", "type2", 20);
            graph.AddEdge("id2", "id4", "type2", 30);

            IEnumerable<Edge<int>> edges = graph.GetEdgesGoingFrom("id1"); 

            Assert.AreEqual(3, edges.Count());
            Assert.AreEqual(2, edges.Where(x => x.From == "id1" && x.To == "id2" && x.Type == "type1" && x.Value == 10).Count());
            Assert.AreEqual(1, edges.Where(x => x.From == "id1" && x.To == "id3" && x.Type == "type2" && x.Value == 20).Count());

            Assert.AreEqual(0, graph.GetEdgesGoingFrom("bla").Count());
        }

        [Test]
        public void GetEdgesGoingTo()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddEdge("id1", "id2", "type1", 10);
            graph.AddEdge("id1", "id2", "type1", 10); // same one
            graph.AddEdge("id3", "id2", "type2", 20);
            graph.AddEdge("id2", "id4", "type2", 30);

            IEnumerable<Edge<int>> edges = graph.GetEdgesGoingTo("id2");

            Assert.AreEqual(3, edges.Count());
            Assert.AreEqual(2, edges.Where(x => x.From == "id1" && x.To == "id2" && x.Type == "type1" && x.Value == 10).Count());
            Assert.AreEqual(1, edges.Where(x => x.From == "id3" && x.To == "id2" && x.Type == "type2" && x.Value == 20).Count());

            Assert.AreEqual(0, graph.GetEdgesGoingTo("bla").Count());
        }

        [Test]
        public void GetEdges()
        {
            Graph<string, int> graph = new Graph<string, int>();
            graph.AddEdge("id1", "id2", "type1", 10);
            graph.AddEdge("id1", "id2", "type1", 10); // same one
            graph.AddEdge("id3", "id2", "type2", 20);
            graph.AddEdge("id2", "id4", "type2", 30);

            IEnumerable<Edge<int>> edges = graph.GetEdges("id1", "id2");

            Assert.AreEqual(2, edges.Count());
            Assert.AreEqual(2, edges.Where(x => x.From == "id1" && x.To == "id2" && x.Type == "type1" && x.Value == 10).Count());

            Assert.AreEqual(0, graph.GetEdges("bla1", "bla2").Count());
        }
    }
}
