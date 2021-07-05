using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishDictionary;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class DirectedGraphExtTest
    {
        private EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void GetPossibleAdTrees()
        {
            var patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.A_Lexeme_Adjective,
                EnglishPattern.O1_I,
                EnglishPattern.A_O,
            };

            var graph = patterns.CreatePatternGraph();

            var result = graph.GetAdTreeFactories(EnglishPattern.O1_I, 2).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("IO", result[0].PatternSignature);

            result = graph.GetAdTreeFactories(EnglishPattern.O1_I, 3).ToList();
            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("IO", result[0].PatternSignature);
            Assert.AreEqual("IAO", result[1].PatternSignature);
            


            patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.O1_I,
                EnglishPattern.O_U_O,
            };
            graph = patterns.CreatePatternGraph();

            result = graph.GetAdTreeFactories(EnglishPattern.O1_I, 4).ToList();
            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("IO", result[0].PatternSignature);
            Assert.AreEqual("IOUO", result[1].PatternSignature);
            


            patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.O1_I,
                EnglishPattern.O_to_O_s,
            };
            graph = patterns.CreatePatternGraph();

            result = graph.GetAdTreeFactories(EnglishPattern.O1_I, 3).ToList();

            // Note: only one of morpheme rules ("O" and "O>O_s") shall be taken into account.
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("IO", result[0].PatternSignature);



            patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Noun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.O2_I,
                EnglishPattern.I_U_I,
            };
            graph = patterns.CreatePatternGraph();

            //graph = PatternProvider.Patterns.CreatePatternGraph();
            
            var signatures = graph.GetAdTreeFactories(EnglishPattern.I_U_I, 7)
                .Select(x => x.PatternSignature)
                .ToList();

            Assert.IsTrue(signatures.Contains("OIOUOIO"));
        }
    }
}
