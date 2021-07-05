using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class PatternsExtTest
    {
        [Test]
        public void CreatePatternGraph()
        {
            var patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Noun,
            };

            var graph = patterns.CreatePatternGraph();
            Assert.AreEqual(1, graph.Count);
            Assert.AreEqual(0, graph.Edges.Count());


            patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.O1_I,
                EnglishPattern.O2_I,
            };

            graph = patterns.CreatePatternGraph();
            Assert.AreEqual(4, graph.Count);
            Assert.AreEqual(4, graph.Edges.Count());

            Assert.AreEqual(1, graph.GetEdgesGoingTo(EnglishPattern.I_Lexeme_Verb).Count());
            Assert.AreEqual(2, graph.GetEdgesGoingTo(EnglishPattern.O_Lexeme_Noun).Count());
            Assert.AreEqual(1, graph.GetEdgesGoingTo(EnglishPattern.O1_I).Count());
            Assert.AreEqual(0, graph.GetEdgesGoingTo(EnglishPattern.O2_I).Count());
        }
    }
}
