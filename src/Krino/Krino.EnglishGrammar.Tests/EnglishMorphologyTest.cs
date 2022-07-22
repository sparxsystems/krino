using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Morphology;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Krino.EnglishGrammar.Tests
{
    [TestFixture]
    public class EnglishMorphologyTest
    {
        [Test]
        public void GetDerivationSequence()
        {
            var morphology = new EnglishMorphology();

            var suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "ing" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));
            var lex = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "eat" && GrammarAttributes.Morpheme.Free.Lexical.Verb.IsIn(x.Attributes));

            var result = morphology.GetDerivationSequence(new[] { lex, suffix }).ToList();
            Assert.AreEqual(1, result.Count);
        }
    }
}
