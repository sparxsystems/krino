using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Morphology;
using NUnit.Framework;
using System.Linq;

namespace Krino.EnglishGrammar.Tests
{
    [TestFixture]
    public class EnglishMorphologyTest
    {
        [Test]
        public void GetDerivationSequence()
        {
            var morphology = new EnglishMorphology();

            var lex = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "eat" && GrammarAttributes.Morpheme.Free.Lexical.Verb.IsIn(x.Attributes));
            var suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "ing" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));

            var result = morphology.GetDerivationSequences(new[] { lex, suffix }).ToList();
            Assert.AreEqual(1, result.Count);


            var prefix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "un" && GrammarAttributes.Morpheme.Bound.Prefix.IsIn(x.Attributes));
            lex = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "happy" && GrammarAttributes.Morpheme.Free.Lexical.Adjective.IsIn(x.Attributes));
            suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "ness" && GrammarAttributes.Morpheme.Bound.Suffix.Derivational.IsIn(x.Attributes));

            result = morphology.GetDerivationSequences(new[] { prefix, lex, suffix }).ToList();
            Assert.AreEqual(2, result.Count);
        }
    }
}
