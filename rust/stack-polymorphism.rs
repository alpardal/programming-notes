struct Object {
    id: Id,
    shape: ShapeType,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ShapeType {
    Sphere(SphereImpl),
    Plane(PlaneImpl),
}
use ShapeType::*;

trait Shape {
    fn local_normal_at(&self, point: &Tuple) -> Tuple;
}

impl Object {
    pub fn normal_at(&self, p: Tuple) -> Tuple {
        let local_p = self.transform.inverse() * p;
        let local_normal =
            self.inner_shape().local_normal_at(&local_p);
        // transform local normal back to world space
    }

    // get reference to stack-allocated shape variant, as a trait instance
    //   - avoids having to use a Box<dyn Shape>
    fn inner_shape(&self) -> &dyn Shape {
        match self.shape {
            Sphere(ref s) => s,
            Plane(ref p) => p,
        }
    }
}
