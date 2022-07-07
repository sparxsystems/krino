using Krino.ConstructiveGrammar.AdTrees;
using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeTest
    {

        [Test]
        public void AdPosition_IncorrectParent_Exception()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Left = new AdTree(new Morpheme("", 0))
            };
            AdTree adTree2 = new AdTree(new Morpheme("", 0));

            // The adTree2 does not references the adTree.RightChild.RightChild therefore it shall throw the exception.
            Assert.Throws<InvalidOperationException>(() => adTree.AdPosition = adTree2);

            // Also it shall not be possible to assign null.
            Assert.Throws<InvalidOperationException>(() => adTree.Left.AdPosition = null);
        }

        [Test]
        public void IsAdPosition()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0)),
                Left = new AdTree(new Morpheme("", 0)),
            };

            Assert.IsTrue(adTree.IsAdPosition);
            Assert.IsFalse(adTree.Right.IsAdPosition);
            Assert.IsFalse(adTree.Left.IsAdPosition);
        }

        [Test]
        public void AdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("hello", 0)),
                    Left = new AdTree(new Morpheme("", 0))
                },
                Left = new AdTree(new Morpheme("", 0)),
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
            AdTree adTree = new AdTree(new Morpheme("", 0));
            Assert.IsNull(adTree.Right);

            AdTree rightChild = new AdTree(new Morpheme("", 0));
            
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
            AdTree adTree = new AdTree(new Morpheme("", 0));
            Assert.IsNull(adTree.Left);

            AdTree leftChild = new AdTree(new Morpheme("", 0));

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
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", 0)),
                    Left = new AdTree(new Morpheme("I", 0))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", 0)),
                    Left = new AdTree(new Morpheme("the", 0))
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
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", 0)),
                    Left = new AdTree(new Morpheme("I", 0))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", 0)),
                    Left = new AdTree(new Morpheme("the", 0))
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
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", 0)),
                    Left = new AdTree(new Morpheme("I", 0))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", 0)),
                    Left = new AdTree(new Morpheme("the", 0))
                }
            };

            Assert.IsFalse(adTree.Right.IsOnLeft);
            Assert.IsTrue(adTree.Left.IsOnLeft);
            Assert.IsFalse(adTree.IsOnLeft);
        }

        [Test]
        public void GetMyGovernor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb)),
                    Left = new AdTree(new Morpheme("I", GrammarAttributes.Morpheme.Free.Functional.Pronoun))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun)),
                    Left = new AdTree(new Morpheme("the", GrammarAttributes.Morpheme.Free.Functional.Determiner))
                }
            };

            // the governor of 'I' is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.Left.GetMyGovernor());

            // the governor of 'book'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.GetMyGovernor());

            // the governor of 'book' is the 'read'.
            // Note: the book is already the governor so the governor of the governor shall be found.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.Right.GetMyGovernor());

            // the governor of 'the' is the 'book'.
            Assert.IsTrue(adTree.Left.Right == adTree.Left.Left.GetMyGovernor());

            // the governor of the root is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.GetMyGovernor());

            // the governor of the 'read'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.GetMyGovernor());
        }

        [Test]
        public void IsGovernor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb)),
                    Left = new AdTree(new Morpheme("I", GrammarAttributes.Morpheme.Free.Functional.Pronoun))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun)),
                    Left = new AdTree(new Morpheme("the", GrammarAttributes.Morpheme.Free.Functional.Determiner))
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
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb)),
                    Left = new AdTree(new Morpheme("I", GrammarAttributes.Morpheme.Free.Functional.Pronoun))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun)),
                    Left = new AdTree(new Morpheme("the", GrammarAttributes.Morpheme.Free.Functional.Determiner))
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
            AdTree adTree = new AdTree(new Morpheme(".", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("", 0))
                    {
                        IsLeftFirst = true,
                        Right = new AdTree(new Morpheme("read", 0)),
                        Left = new AdTree(new Morpheme("I", 0))
                    },
                    Left = new AdTree(new Morpheme("", 0))
                    {
                        IsLeftFirst = true,
                        Right = new AdTree(new Morpheme("book", 0)),
                        Left = new AdTree(new Morpheme("the", 0))
                    }
                }
            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("I read the book .", phrase);
        }

        [Test]
        public void Phrase_Suffix()
        {
            AdTree adTree = new AdTree(new Morpheme("", GrammarAttributes.Morpheme.Free.Lexical.Noun))
            {
                Right = new AdTree(new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb)),
                Left = new AdTree(new Morpheme("ing", GrammarAttributes.Morpheme.Bound.Suffix)),
            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("reading", phrase);
        }
    }
}
