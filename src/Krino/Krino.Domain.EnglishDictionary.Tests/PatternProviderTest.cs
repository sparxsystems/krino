using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System;
using System.Linq;

namespace Krino.Domain.EnglishDictionary.Tests
{
    [TestFixture]
    public class Tests
    {
        [Test]
        public void PluralNoun()
        {
            var dictionary = new ConstructiveDictionary(MorphemeProvider.AttributesModel, MorphemeProvider.Morphemes, PatternProvider.Patterns);
            var adTreeCreator = new AdTreeCreator(dictionary);
            var results = adTreeCreator.Create("climates");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("O>O_s", results[0].Pattern.Name);
        }

        [Test]
        public void NounFromVerb()
        {
            var dictionary = new ConstructiveDictionary(MorphemeProvider.AttributesModel, MorphemeProvider.Morphemes, PatternProvider.Patterns);
            var adTreeCreator = new AdTreeCreator(dictionary);
            var results = adTreeCreator.Create("walking");

            Assert.IsTrue(results.Count > 0);
            Assert.AreEqual(1, results.Count(x => x.Pattern.Name == "I>O_ing"));
        }

        [Test]
        public void AdjectiveFromVerb()
        {
            var dictionary = new ConstructiveDictionary(MorphemeProvider.AttributesModel, MorphemeProvider.Morphemes, PatternProvider.Patterns);
            var adTreeCreator = new AdTreeCreator(dictionary);
            var results = adTreeCreator.Create("prohibited");
            
            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("I>A_ed", results[0].Pattern.Name);
        }

        [Test]
        public void GetPossibleAdTrees_EnglishPattern()
        {
            var graph = PatternProvider.Patterns.CreatePatternGraph();

            var count = 0;
            var result = graph.GetPossibleAdTrees(EnglishPattern.I_Period_I, MorphemeProvider.AttributesModel, 9)
                .Select(x =>
                {
                    var signature = x.PatternSignature;
                    ++count;
                    return x;
                })
                .ToList();
        }
    }
}