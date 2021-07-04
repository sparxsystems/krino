using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.ConstructiveDictionaries
{
    [TestFixture]
    public class PatternsExtTest
    {
        [Test]
        public void CreatePatternGraph()
        {
            var patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme,
            };

            var graph = patterns.CreatePatternGraph();
            Assert.AreEqual(1, graph.Count);
            Assert.AreEqual(0, graph.Edges.Count());


            patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme,
                EnglishPattern.I_Lexeme,
                EnglishPattern.O1_I,
                EnglishPattern.O2_I,
            };

            graph = patterns.CreatePatternGraph();
            Assert.AreEqual(4, graph.Count);
            Assert.AreEqual(4, graph.Edges.Count());

            Assert.AreEqual(1, graph.GetEdgesGoingTo(EnglishPattern.I_Lexeme).Count());
            Assert.AreEqual(2, graph.GetEdgesGoingTo(EnglishPattern.O_Lexeme).Count());
            Assert.AreEqual(1, graph.GetEdgesGoingTo(EnglishPattern.O1_I).Count());
            Assert.AreEqual(0, graph.GetEdgesGoingTo(EnglishPattern.O2_I).Count());
        }
    }
}
