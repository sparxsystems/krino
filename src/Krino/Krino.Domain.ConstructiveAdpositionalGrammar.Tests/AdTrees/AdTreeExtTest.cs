using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeExtTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void GetSequenceToRoot()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            List<IAdTree> sequence = adTree.Right.Left.GetSequenceToRoot().ToList();
            Assert.AreEqual(3, sequence.Count);
            Assert.IsTrue(adTree.Right.Left == sequence[0]);
            Assert.IsTrue(adTree.Right == sequence[1]);
            Assert.IsTrue(adTree == sequence[2]);
        }

        [Test]
        public void GetPath()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            AdTreePosition[] path = adTree.Right.Left.GetPath();
            Assert.AreEqual(2, path.Length);
            Assert.AreEqual(AdTreePosition.ChildOnRight, path[0]);
            Assert.AreEqual(AdTreePosition.ChildOnLeft, path[1]);

            // Root
            path = adTree.GetPath();
            Assert.AreEqual(0, path.Length);
        }

        [Test]
        public void TryGetAdTree()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            adTree.TryGetAdTree(new AdTreePosition[] { AdTreePosition.ChildOnRight, AdTreePosition.ChildOnLeft }, out IAdTree result);
            Assert.AreEqual("I", result.Morpheme.Morph);

            // Root
            adTree.TryGetAdTree(new AdTreePosition[] { }, out IAdTree root);
            Assert.IsTrue(adTree == root);
        }

        [Test]
        public void GetFirstAdPositionOnLeft()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            IAdTree result = adTree.Left.Right.GetFirstAdPositionOnLeft();
            Assert.IsTrue(adTree.Left == result);

            // It is already on left.
            result = adTree.Right.Left.GetFirstAdPositionOnLeft();
            Assert.IsTrue(adTree.Right.Left == result);

            // Root.
            result = adTree.GetFirstAdPositionOnLeft();
            Assert.IsNull(result);
        }

        [Test]
        public void MakeDeepCopy()
        {
            // The phrase: I read the book.
            var adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.U), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme), new Pattern())
                }
            };

            var copy = adTree.Right.MakeDeepCopy();
            Assert.IsNull(copy.AdPosition);
            Assert.IsTrue(adTree.Right.Equals(copy));
        }

        [Test]
        public void MakeShallowCopy()
        {
            // The phrase: I read the book.
            IAdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.U), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme), new Pattern())
                }
            };

            // Note: the copy does not start from the root but the whole tree will be copied.
            IAdTree copy = adTree.Right.MakeShallowCopy();

            // The copy must be set to the same position as the original.
            Assert.IsTrue(adTree.Right.Morpheme == copy.Morpheme);
            Assert.IsTrue(adTree.Right.Pattern == copy.Pattern);

            // Get the root - for easier testing.
            adTree = adTree.Root;
            copy = copy.Root;

            Assert.IsTrue(adTree.Morpheme == copy.Morpheme);
            Assert.IsTrue(adTree.Pattern == copy.Pattern);

            Assert.IsTrue(adTree.Right.Morpheme == copy.Right.Morpheme);
            Assert.IsTrue(adTree.Right.Pattern == copy.Right.Pattern);

            Assert.IsTrue(adTree.Right.Right.Morpheme == copy.Right.Right.Morpheme);
            Assert.IsTrue(adTree.Right.Right.Pattern == copy.Right.Right.Pattern);

            Assert.IsTrue(adTree.Right.Left.Morpheme == copy.Right.Left.Morpheme);
            Assert.IsTrue(adTree.Right.Left.Pattern == copy.Right.Left.Pattern);

            Assert.IsTrue(adTree.Left.Morpheme == copy.Left.Morpheme);
            Assert.IsTrue(adTree.Left.Pattern == copy.Left.Pattern);

            Assert.IsTrue(adTree.Left.Right.Morpheme == copy.Left.Right.Morpheme);
            Assert.IsTrue(adTree.Left.Right.Pattern == copy.Left.Right.Pattern);

            Assert.IsTrue(adTree.Left.Left.Morpheme == copy.Left.Left.Morpheme);
            Assert.IsTrue(adTree.Left.Left.Pattern == copy.Left.Left.Pattern);
        }

        
    }
}
