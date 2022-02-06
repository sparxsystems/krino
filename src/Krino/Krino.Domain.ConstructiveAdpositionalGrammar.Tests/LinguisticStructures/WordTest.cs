using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticStructures
{
    [TestFixture]
    public class WordTest
    {
        [Test]
        public void Value()
        {
            var word = new Word("book", GrammarAttributes.Morpheme.O.Free.Noun);
            Assert.AreEqual("book", word.Value);

            word = new Word("read", GrammarAttributes.Morpheme.I.Free.Verb);
            word.Suffixes.Add(new Morpheme("ing", GrammarAttributes.Morpheme.O.Bound.Suffix));
            Assert.AreEqual("reading", word.Value);
        }
    }
}
