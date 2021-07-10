using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class PatternConstructionsExtTest
    {
        private EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

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


        [Test]
        public void GetAdTreeFactoriesForPatternSignature()
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
            var buffer = new PatternConstructionsBuffer(graph, 3);

            var result = graph.GetAdTreeFactoriesForPatternSignature("IAO", buffer).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("IAO", result[0].PatternSignature);
        }


        [Test]
        public void GetAdTreeFactories()
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

            var signatures = graph.GetAdTreeFactories(EnglishPattern.I_U_I, 7)
                .Select(x => x.PatternSignature)
                .ToList();

            Assert.IsTrue(signatures.Contains("OIOUOIO"));


            patterns = new List<Pattern>()
            {
                EnglishPattern.O_Lexeme_Pronoun,
                EnglishPattern.I_Lexeme_Verb,
                EnglishPattern.I_Lexeme_Verb_Will,
                EnglishPattern.O1_I.SetLeftFirst(),
                EnglishPattern.Will_I,
            };
            graph = patterns.CreatePatternGraph();

            signatures = graph.GetAdTreeFactories(EnglishPattern.O1_I.SetLeftFirst(), 3)
                .Select(x => x.PatternSignature)
                .ToList();

            Assert.IsTrue(signatures.Contains("OII"));


            //patterns = new List<Pattern>()
            //{
            //    EnglishPattern.O_Lexeme_Pronoun, // I
            //    //EnglishPattern.I_Lexeme_Verb, // have
            //    EnglishPattern.I_Lexeme_Verb_Been, // been
            //    EnglishPattern.O1_I.SetLeftFirst(),
            //    EnglishPattern.I_Suffix_ing, //-ing
            //    EnglishPattern.Have_I, // have + (been + (read + ing))
            //    EnglishPattern.Been_I_ing, // been + (read + ing)
            //    EnglishPattern.I_to_I_ing, // read + ing
            //};
            //graph = patterns.CreatePatternGraph();

            //signatures = graph.GetAdTreeFactories(EnglishPattern.O1_I.SetLeftFirst(), 5)
            //    .Select(x => x.PatternSignature)
            //    .ToList();

            //Assert.IsTrue(signatures.Contains("OIII"));
        }
    }
}
