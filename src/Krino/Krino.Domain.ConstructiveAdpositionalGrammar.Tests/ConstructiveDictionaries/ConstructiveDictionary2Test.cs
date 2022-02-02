using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.ConstructiveDictionaries
{
    [TestFixture]
    public class ConstructiveDictionary2Test
    {
        [Test]
        public void FindFreeMorphemes_Similar()
        {
            var morphemes = new List<Morpheme>()
            {
                new Morpheme("write", GrammarAttributes.Morpheme.I.Free.Verb),
                new Morpheme("book", GrammarAttributes.Morpheme.O.Free.Noun),
            };

            var dictionary = new ConstructiveDictionary2(morphemes);

            var result = dictionary.FindFreeMorphemes("writ", 1).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("write", result[0].Value);

            // Try not similar but exactly matching morph.
            // Note: it cannot return two same morphemes (one exactly matching and then
            //       again the same morpheme as the similar one) but only one morpheme.
            result = dictionary.FindFreeMorphemes("write", 1).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("write", result[0].Value);
        }
    }
}
