using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes
{
    [TestFixture]
    public class MorphemeTest
    {
        [Test]
        public void GrammarCharacter_Property()
        {
            IAttributesModel attributesModel = new EnglishAttributesModel();

            Morpheme morpheme = new Morpheme(attributesModel, "hello", EnglishAttributes.A.Lexeme);
            Assert.AreEqual(GrammarCharacter.A, morpheme.GrammarCharacter);
        }
    }
}
