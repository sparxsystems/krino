using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.EnglishGrammar.Morphology;
using NUnit.Framework;

namespace Krino.ConstructiveGrammar.Tests.LinguisticStructures
{
    [TestFixture]
    public class WordTest
    {
        [Test]
        public void Value()
        {
            var morphology = new EnglishMorphology();

            var word = new Word(morphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            Assert.AreEqual("book", word.Value);

            word = new Word(morphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb);
            word.Suffixes.Add(new Morpheme("ing", GrammarAttributes.Morpheme.Bound.Suffix));
            Assert.AreEqual("reading", word.Value);
        }
    }
}
