using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes
{
    [TestFixture]
    public class MorphemeTest
    {
        [Test]
        public void GrammarCharacter_Property()
        {
            Morpheme morpheme = new Morpheme("hello", Attributes.A.Lexeme);
            Assert.AreEqual(GrammarCharacter.A, morpheme.GrammarCharacter);
        }
    }
}
