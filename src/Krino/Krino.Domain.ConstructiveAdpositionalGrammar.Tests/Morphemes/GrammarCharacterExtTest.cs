using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes
{
    [TestFixture]
    public class GrammarCharacterExtTest
    {
        [Test]
        public void GetGrammarCharacter()
        {
            Assert.AreEqual(GrammarCharacter.O, GrammarCharacterExt.GetGrammarCharacter(Attributes.O));
            Assert.AreEqual(GrammarCharacter.I, GrammarCharacterExt.GetGrammarCharacter(Attributes.I));
            Assert.AreEqual(GrammarCharacter.A, GrammarCharacterExt.GetGrammarCharacter(Attributes.A));
            Assert.AreEqual(GrammarCharacter.E, GrammarCharacterExt.GetGrammarCharacter(Attributes.E));
            Assert.AreEqual(GrammarCharacter.U, GrammarCharacterExt.GetGrammarCharacter(Attributes.U));

            Assert.AreEqual(GrammarCharacter.e, GrammarCharacterExt.GetGrammarCharacter(0));
        }
    }
}
