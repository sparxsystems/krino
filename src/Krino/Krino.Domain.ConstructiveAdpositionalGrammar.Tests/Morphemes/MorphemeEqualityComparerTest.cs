using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes
{
    [TestFixture]
    public class MorphemeEqualityComparerTest
    {
        [Test]
        public void Equals()
        {
            var attributesModel = new EnglishAttributesModel();

            MorphemeEqualityComparer comparer = new MorphemeEqualityComparer();
            
            Assert.IsTrue(comparer.Equals(
                new Morpheme(attributesModel, "room", EnglishAttributes.O.Lexeme), 
                new Morpheme(attributesModel, "room", EnglishAttributes.O.Lexeme)));

            Assert.IsFalse(comparer.Equals(
                new Morpheme(attributesModel, "bla", EnglishAttributes.O.Lexeme),
                new Morpheme(attributesModel, "room", EnglishAttributes.O.Lexeme)));

            Assert.IsFalse(comparer.Equals(
                new Morpheme(attributesModel, "room", EnglishAttributes.O.Lexeme),
                new Morpheme(attributesModel, "room", EnglishAttributes.A.Lexeme)));
        }
    }
}
