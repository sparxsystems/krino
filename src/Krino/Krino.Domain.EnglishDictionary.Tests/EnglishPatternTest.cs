using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using NUnit.Framework;

namespace Krino.Domain.EnglishDictionary.Tests
{
    [TestFixture]
    public class EnglishPatternTest
    {
        [Test]
        public void I_to_A_ed()
        {
            Assert.IsTrue(EnglishPattern.I_to_A_ed.IsBipolarMorphemeTransference);
            Assert.AreEqual(GrammarCharacter.A, EnglishPattern.I_to_A_ed.RulingGrammarCharacter);
        }
    }
}
