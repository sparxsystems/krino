using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using NUnit.Framework;

namespace Krino.ConstructiveGrammar.Tests.LinguisticStructures
{
    [TestFixture]
    public class WordTest
    {
        [Test]
        public void Value()
        {
            var word = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            Assert.AreEqual("book", word.Value);

            word = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb);
            word.Suffixes.Add(new Morpheme("ing", GrammarAttributes.Morpheme.Bound.Suffix));
            Assert.AreEqual("reading", word.Value);
        }
    }
}
