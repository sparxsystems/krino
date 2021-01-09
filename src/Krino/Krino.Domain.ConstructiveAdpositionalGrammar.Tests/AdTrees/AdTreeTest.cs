using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void InheritedGrammarCharacter()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), new Pattern())
                }
            };


            Assert.AreEqual(GrammarCharacter.I, adTree.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.e, adTree.Right.Left.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.I, adTree.Right.InheritedGrammarCharacter);

            Assert.AreEqual(GrammarCharacter.O, adTree.Left.InheritedGrammarCharacter);
        }

        [Test]
        public void AdPosition_IncorrectParent_Exception()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            };
            AdTree adTree2 = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern());

            // The adTree2 does not references the adTree.RightChild.RightChild therefore it shall throw the exception.
            Assert.Throws<InvalidOperationException>(() => adTree.AdPosition = adTree2);

            // Also it shall not be possible to assign null.
            Assert.Throws<InvalidOperationException>(() => adTree.Left.AdPosition = null);
        }

        [Test]
        public void IsAdPosition()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
            };

            Assert.IsTrue(adTree.IsAdPosition);
            Assert.IsFalse(adTree.Right.IsAdPosition);
            Assert.IsFalse(adTree.Left.IsAdPosition);
        }

        [Test]
        public void AdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
            };

            List<IAdTree> adPositions = adTree.AdPositions.ToList();
            Assert.AreEqual(0, adPositions.Count);

            adPositions = adTree.Right.Right.AdPositions.ToList();
            Assert.AreEqual(2, adPositions.Count);
            Assert.AreEqual(adTree.Right, adPositions[0]);
            Assert.AreEqual(adTree, adPositions[1]);
        }


        [Test]
        public void RightChild()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern());
            Assert.IsNull(adTree.Right);

            AdTree rightChild = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern());
            
            // Attach the governor.
            adTree.Right = rightChild;
            Assert.AreEqual(rightChild, adTree.Right);
            Assert.AreEqual(adTree, rightChild.AdPosition);

            // Detach the governor.
            adTree.Right = null;
            Assert.IsNull(adTree.Right);
            Assert.IsNull(rightChild.AdPosition);
        }

        [Test]
        public void LeftChild()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern());
            Assert.IsNull(adTree.Left);

            AdTree leftChild = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern());

            // Attach dependent.
            adTree.Left = leftChild;
            Assert.AreEqual(leftChild, adTree.Left);
            Assert.AreEqual(adTree, leftChild.AdPosition);

            // Detach the dependent.
            adTree.Left = null;
            Assert.IsNull(adTree.Left);
            Assert.IsNull(leftChild.AdPosition);
        }


        [Test]
        public void RightChildren()
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

            List<IAdTree> rightChildren = adTree.RightChildren.ToList();
            Assert.AreEqual(2, rightChildren.Count);
            Assert.AreEqual(adTree.Right, rightChildren[0]);
            Assert.AreEqual(adTree.Right.Right, rightChildren[1]);


            rightChildren = adTree.Right.Right.RightChildren.ToList();
            Assert.AreEqual(0, rightChildren.Count);
        }

        [Test]
        public void IsOnRight()
        {
            // The phrase: I read the book.
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

            Assert.IsTrue(adTree.Right.IsOnRight);
            Assert.IsFalse(adTree.Left.IsOnRight);
            Assert.IsFalse(adTree.IsOnRight);
        }

        [Test]
        public void IsOnLeft()
        {
            // The phrase: I read the book.
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

            Assert.IsFalse(adTree.Right.IsOnLeft);
            Assert.IsTrue(adTree.Left.IsOnLeft);
            Assert.IsFalse(adTree.IsOnLeft);
        }

        [Test]
        public void Governor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I), EnglishPattern.Morpheme(EnglishAttributes.I)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O))
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.Morpheme(EnglishAttributes.A))
                }
            };

            // the governor of 'I' is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.Left.GetGovernor());

            // the governor of 'book'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.GetGovernor());

            // the governor of 'book' is the 'read'.
            // Note: the book is already the governor so the governor of the governor shall be found.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.Right.GetGovernor());

            // the governor of 'the' is the 'book'.
            Assert.IsTrue(adTree.Left.Right == adTree.Left.Left.GetGovernor());

            // the governor of the root is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.GetGovernor());

            // the governor of the 'read'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.GetGovernor());
        }

        [Test]
        public void IsGovernor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I), EnglishPattern.Morpheme(EnglishAttributes.I)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O))
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.Morpheme(EnglishAttributes.A))
                }
            };

            // 'read' is governor.
            Assert.IsTrue(adTree.Right.Right.IsGovernor);

            // 'book'-adposition is not the governor.
            Assert.IsFalse(adTree.Left.IsGovernor);

            // root is not the governor.
            Assert.IsFalse(adTree.IsGovernor);
        }

        [Test]
        public void DependentAdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I), EnglishPattern.Morpheme(EnglishAttributes.I)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O))
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.Morpheme(EnglishAttributes.A))
                }
            };

            // dependents for 'read'
            List<IAdTree> dependentAdPositions = adTree.Right.Right.DependentAdPositions.ToList();
            Assert.AreEqual(2, dependentAdPositions.Count);
            Assert.IsTrue(adTree.Right == dependentAdPositions[0]);
            Assert.IsTrue(adTree == dependentAdPositions[1]);

            // dependents for 'book'
            dependentAdPositions = adTree.Left.Right.DependentAdPositions.ToList();
            Assert.AreEqual(1, dependentAdPositions.Count);
            Assert.IsTrue(adTree.Left == dependentAdPositions[0]);
        }


        [Test]
        public void Phrase()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, ".", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "", 0),
                        new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme.SetOrder(1) })
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                        Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                    },
                    Left = new AdTree(new Morpheme(myAttributesModel, "", 0),
                        new Pattern() { LeftRule = EnglishMorphemeRule.A_Lexeme.SetOrder(1) })
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                        Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                    }
                }
            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("I read the book .", phrase);
        }
    }
}
