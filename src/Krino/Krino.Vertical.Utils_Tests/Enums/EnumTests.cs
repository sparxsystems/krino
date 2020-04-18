using Krino.Vertical.Utils.Enums;
using NUnit.Framework;
using System;

namespace Krino.Vertical.Utils_Tests.Enums
{
    [TestFixture]
    public class EnumTests
    {
        public class DummyEnumRoot : EnumRootGroupBase
        {
            private DummyEnumRoot() : base(2) { }

            public static DummyEnumRoot Instance { get; } = new DummyEnumRoot();

            public static DummyCategory1 Category1 { get; } = new DummyCategory1(Instance, 1);

            public static EnumValue Val0 { get; } = new EnumValue(Instance, 2);

        }

        public class DummyCategory1 : EnumGroupBase
        {
            public DummyCategory1(EnumGroupBase parent, int localPosition) : base(parent, 2, localPosition)
            {
                Category2 = new DummyCategory2(this, 1);
                Val1 = new EnumValue(this, 2);
            }

            public DummyCategory2 Category2 { get; }
            public EnumValue Val1 { get; }
        }

        public class DummyCategory2 : EnumGroupBase
        {
            public DummyCategory2(EnumGroupBase parent, int localPosition) : base(parent, 3, localPosition)
            {
                Attr1 = new EnumValue(this, 1);
                Attr2 = new EnumValue(this, 2);
                Attr3 = new EnumValue(this, 3);
            }

            public EnumValue Attr1 { get; }
            public EnumValue Attr2 { get; }
            public EnumValue Attr3 { get; }
        }


        public class ExceedingGroupLength : EnumGroupBase
        {
            public ExceedingGroupLength(EnumGroupBase parent, int localPosition) : base(parent, 2, localPosition)
            {
                // The group length is 2 but here are three enum properties.
                Attr1 = new EnumValue(this, 1);
                Attr2 = new EnumValue(this, 2);
                Attr3 = new EnumValue(this, 3);
            }

            public EnumValue Attr1 { get; }
            public EnumValue Attr2 { get; }
            public EnumValue Attr3 { get; }
        }


        [Test]
        public void EncodedValue_ulong()
        {
            //                                 01        01        010
            ulong encodedValue = DummyEnumRoot.Category1.Category2.Attr2;
            Assert.AreEqual(0b010_01_01, encodedValue);

            //                           01        10
            encodedValue = DummyEnumRoot.Category1.Val1;
            Assert.AreEqual(0b10_01, encodedValue);

            //                           01        10        100
            encodedValue = DummyEnumRoot.Category1.Category2.Attr3;
            Assert.AreEqual(0b100_01_01, encodedValue);
        }

        [Test]
        public void IsIn()
        {
            ulong encodedValue = DummyEnumRoot.Category1.Category2.Attr2 | DummyEnumRoot.Val0;

            Assert.IsTrue(DummyEnumRoot.Category1.IsIn(encodedValue));
            Assert.IsTrue(DummyEnumRoot.Category1.Category2.IsIn(encodedValue));
            Assert.IsTrue(DummyEnumRoot.Category1.Category2.Attr2.IsIn(encodedValue));

            Assert.IsTrue(DummyEnumRoot.Val0.IsIn(encodedValue));

            Assert.IsFalse(DummyEnumRoot.Category1.Val1.IsIn(encodedValue));
            Assert.IsFalse(DummyEnumRoot.Category1.Category2.Attr1.IsIn(encodedValue));
            Assert.IsFalse(DummyEnumRoot.Category1.Category2.Attr3.IsIn(encodedValue));
        }

        [Test]
        public void ExceedingCapacity_of_Group()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => new ExceedingGroupLength(DummyEnumRoot.Instance, 1));
            Assert.Throws<ArgumentOutOfRangeException>(() => new ExceedingGroupLength(null, 0));
        }

        public class DummyEnumRoot62 : EnumRootGroupBase
        {
            public DummyEnumRoot62() : base(62) { }
        }
        public class DummyEnum2 : EnumGroupBase
        {
            public DummyEnum2(EnumGroupBase parent, int localPosition) : base(parent, 2, localPosition) { }
        }
        public class DummyEnum3 : EnumGroupBase
        {
            public DummyEnum3(EnumGroupBase parent, int localPosition) : base(parent, 3, localPosition) { }
        }

        [Test]
        public void Capacity_of_uint()
        {
            // 3 + 61 = 64 -> OK
            new DummyEnum2(new DummyEnumRoot62(), 1);

            // 3 + 62 = 65 -> exception
            Assert.Throws<ArgumentOutOfRangeException>(() => new DummyEnum3(new DummyEnumRoot62(), 1));
        }
    }
}
