using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes
{
    [TestFixture]
    public class MorphemeEqualityComparerTest
    {
        [Test]
        public void Equals()
        {
            MorphemeEqualityComparer comparer = new MorphemeEqualityComparer();
            
            Assert.IsTrue(comparer.Equals(
                new Morpheme("room", Attributes.O.Lexeme.Noun), 
                new Morpheme("room", Attributes.O.Lexeme.Noun)));

            Assert.IsFalse(comparer.Equals(
                new Morpheme("bla", Attributes.O.Lexeme.Noun),
                new Morpheme("room", Attributes.O.Lexeme.Noun)));

            Assert.IsFalse(comparer.Equals(
                new Morpheme("room", Attributes.O.Lexeme.Noun),
                new Morpheme("room", Attributes.O.Lexeme.Noun.Countable)));
        }
    }
}
