using Krino.Vertical.Utils.StateMachines;
using NUnit.Framework;
using System;
using System.Linq;

namespace Krino.Vertical.Utils_Tests.StateMachines
{
    [TestFixture]
    public class MultiMachineTest
    {
        [Test]
        public void Reset()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("a");
            machine.AddState("b");
            machine.AddFinalState("c");

            Assert.AreEqual(0, machine.ActiveStateRecords.Count());
            Assert.AreEqual(0, machine.UnhandledStateRecords.Count());

            machine.Reset();

            var activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(1, activeStates.Count);
            Assert.AreEqual("a", activeStates[0].Value.Definition.Value);
        }

        [Test]
        public void Reset_MultipleInitialStates()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("a");
            machine.AddInitialState("b");
            machine.AddState("c");
            machine.AddFinalState("d");

            Assert.AreEqual(0, machine.ActiveStateRecords.Count());

            machine.Reset();

            var activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(2, activeStates.Count);
            Assert.AreEqual("a", activeStates[0].Value.Definition.Value);
            Assert.AreEqual("b", activeStates[1].Value.Definition.Value);
        }

        [Test]
        public void Fire()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("a");
            machine.AddState("b");
            machine.AddFinalState("c");
            machine.AddTransition("a", "b", 2);
            machine.AddTransition("a", "c", 3);
            machine.AddTransition("b", "c", 3);
            machine.Reset();

            machine.Fire(2);
            var activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(1, activeStates.Count);
            Assert.AreEqual("b", activeStates[0].Value.Definition.Value);
            Assert.AreEqual(StateKind.Custom, activeStates[0].Value.Definition.StateKind);

            machine.Fire(3);
            activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(1, activeStates.Count);
            Assert.AreEqual("c", activeStates[0].Value.Definition.Value);
            Assert.AreEqual(StateKind.Final, activeStates[0].Value.Definition.StateKind);
            Assert.AreEqual(3, activeStates[0].Value.ByTrigger);

            var track = activeStates[0].Parents.ToList();
            Assert.AreEqual(2, track.Count);
            Assert.AreEqual("b", track[0].Value.Definition.Value);
            Assert.AreEqual(2, track[0].Value.ByTrigger);
            Assert.AreEqual("a", track[1].Value.Definition.Value);
            Assert.IsNull(track[1].Value.ByTransitionRule);
        }

        [Test]
        public void Fire_Unhandled()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("a");
            machine.AddState("b");
            machine.AddFinalState("c");
            machine.AddTransition("a", "b", 2);
            machine.AddTransition("a", "c", 3);
            machine.AddTransition("b", "c", 3);
            machine.Reset();

            machine.Fire(100);
            
            var activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(0, activeStates.Count);

            var unhandledStates = machine.UnhandledStateRecords.ToList();
            Assert.AreEqual(1, unhandledStates.Count);
            Assert.AreEqual("a", unhandledStates[0].Value.Definition.Value);
            Assert.AreEqual(StateKind.Initial, unhandledStates[0].Value.Definition.StateKind);
            Assert.IsTrue(unhandledStates[0].Value.IsUnhandled);
            Assert.AreEqual(100, unhandledStates[0].Value.UnhandledTrigger);
        }

        [Test]
        public void Fire_MultipleActiveStates()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("a");
            machine.AddState("b");
            machine.AddFinalState("c");
            machine.AddTransition("a", "b", 2);
            machine.AddTransition("a", "c", 2);
            machine.AddTransition("b", "c", 3);
            machine.Reset();

            machine.Fire(2);
            var activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(2, activeStates.Count);
            Assert.AreEqual("b", activeStates[0].Value.Definition.Value);
            Assert.AreEqual("c", activeStates[1].Value.Definition.Value);
        }

        [Test]
        public void Fire_ImmediateTransition()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("a");
            machine.AddState("b");
            machine.AddFinalState("c");
            machine.AddTransition("a", "b", 2);
            // Immediate transition.
            machine.AddTransition("b", "c");
            machine.Reset();

            machine.Fire(2);
            var activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(1, activeStates.Count);
            Assert.AreEqual("c", activeStates[0].Value.Definition.Value);
        }

        [Test]
        public void Fire_ImmediateTransition_MixedState()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("a");
            machine.AddState("b");
            machine.AddFinalState("c");
            machine.AddFinalState("d");

            machine.AddTransition("a", "b", 2);
            
            // Immediate transition.
            machine.AddTransition("b", "c");

            // ... but also non-immediate transition.
            machine.AddTransition("b", "d", 3);

            machine.Reset();

            machine.Fire(2);

            var activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(2, activeStates.Count);
            Assert.AreEqual("b", activeStates[0].Value.Definition.Value);
            Assert.AreEqual("c", activeStates[1].Value.Definition.Value);
        }

        [Test]
        public void Fire_ImmediateTransition_EndlessLoop()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("a");
            machine.AddState("b");
            machine.AddFinalState("c");
            machine.AddTransition("a", "b", 2);

            // Immediate transition.
            machine.AddTransition("b", "c");
            machine.AddTransition("c", "b");

            machine.Reset();

            Assert.Throws<InvalidOperationException>(() => machine.Fire(2));
        }

        [Test]
        public void Fire_SubStates()
        {
            var machine = new MultiMachine<string, int>();
            machine.AddInitialState("i");

            machine.AddState("a");
            machine.AddInitialSubState("a", "aa");
            machine.AddSubState("a", "ab");
            machine.AddFinalSubState("a", "ac");
            
            machine.AddState("b");
            machine.AddInitialSubState("b", "ba");
            machine.AddFinalSubState("b", "bb");

            machine.AddTransition("i", "a");
            machine.AddTransition("a", "b", 20);
            machine.AddTransition("aa", "ab", 11);
            machine.AddTransition("ab", "ac", 12);

            machine.AddTransition("ba", "bb", 21);

            machine.Reset();

            machine.Fire(11);
            machine.Fire(12);
            machine.Fire(20);
            machine.Fire(21);

            var activeStates = machine.ActiveStateRecords.ToList();
            Assert.AreEqual(1, activeStates.Count);
            Assert.AreEqual("bb", activeStates[0].Value.Definition.Value);
            Assert.AreEqual(StateKind.Final, activeStates[0].Value.Definition.StateKind);

            var track = activeStates[0].Parents.ToList();
            Assert.AreEqual(5, track.Count);
            Assert.AreEqual("ba", track[0].Value.Definition.Value);
            Assert.AreEqual("ac", track[1].Value.Definition.Value);
            Assert.AreEqual("ab", track[2].Value.Definition.Value);
            Assert.AreEqual("aa", track[3].Value.Definition.Value);
            Assert.AreEqual("i", track[4].Value.Definition.Value);
        }
    }
}
