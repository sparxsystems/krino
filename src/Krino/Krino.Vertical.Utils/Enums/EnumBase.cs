using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Represents a group in a structured enum.
    /// </summary>
    /// <remarks>
    /// This class is intended to derive from when a group is needed in the structured enum.
    /// </remarks>
    [DebuggerDisplay("{(ulong)this}")]
    public abstract class EnumBase
    {
        private const ulong HIGHEST_BIT = 0x80_00_00_00_00_00_00_00;
        private ulong myValue;

        /// <summary>
        /// Instantiates the enum.
        /// </summary>
        /// <param name="parent">Parent enum group.</param>
        /// <param name="globalIndex">Index within the whole bit array.</param>
        protected EnumBase(EnumGroupBase parent)
        {
            if (parent != null)
            {
                ParentEnum = parent;

                EnumRootBase root = ParentEnums.OfType<EnumRootBase>().LastOrDefault();
                if (root == null)
                {
                    throw new InvalidOperationException($"Could not instantiate the enum because the root enum of type {nameof(EnumRootBase)} is missing.");
                }

                int globalIndexOfThisEnum = root.Length;

                myValue = parent | (HIGHEST_BIT >> globalIndexOfThisEnum);

                // Increase number of registered enums.
                ++root.Length;
            }
        }

        public EnumGroupBase ParentEnum { get; private set; }

        public IEnumerable<EnumGroupBase> ParentEnums
        {
            get
            {
                EnumGroupBase parent = ParentEnum;
                while (parent != null)
                {
                    yield return parent;
                    parent = parent.ParentEnum;
                }
            }
        }

        private EnumRootBase EnumRoot { get { return ParentEnum == null ? this as EnumRootBase : ParentEnums.OfType<EnumRootBase>().Last(); } }


        /// <summary>
        /// True if the value encodes this enum.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool IsIn(ulong value) => IsIn(myValue, value);

        /// <summary>
        /// True if the tested value encodes the expected value.
        /// </summary>
        /// <param name="expected"></param>
        /// <param name="testedValue"></param>
        /// <returns></returns>
        public static bool IsIn(ulong expected, ulong testedValue) => (testedValue & expected) == expected;


        /// <summary>
        /// Implicitly converts the enum into the ulong.
        /// </summary>
        /// <param name="attribute"></param>
        public static implicit operator ulong(EnumBase attribute) => attribute.myValue;
    }
}
