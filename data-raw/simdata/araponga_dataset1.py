import bpy, os, math, csv
from mathutils import Matrix, Vector
from bpy_extras.object_utils import world_to_camera_view

OUTDIR = "/Users/jocateme/Documents/Projetos/Bellbirds/araponga_dataset"
CSVPATH = os.path.join(OUTDIR, "sim_data.csv")
img_size = (2200, 2200)
use_orthographic_camera = True

# Pyramid geometry
base_radius = 0.4
height = 2.0

# Rotations
interval = 1
pitches = list(range(-90, 90 + 1, interval))
yaws    = list(range(-180 + interval, 180 + 1, interval))
rolls   = list(range(-90, 90 + 1, interval))

os.makedirs(OUTDIR, exist_ok=True)

# --- Start with a clean scene ---
bpy.ops.wm.read_factory_settings(use_empty=True)
scene = bpy.context.scene

# --- Place camera ---
cam_data = bpy.data.cameras.new("Cam")
cam = bpy.data.objects.new("Cam", cam_data)
bpy.context.collection.objects.link(cam)
cam.location = Vector((0.0, -6.0, 0.0))
def look_at(obj, target):
    direction = (target - obj.location).normalized()
    rot_quat = direction.to_track_quat('-Z', 'Y')
    obj.rotation_euler = rot_quat.to_euler()
look_at(cam, Vector((0,0,0)))

if use_orthographic_camera:
    cam.data.type = 'ORTHO'
    cam.data.ortho_scale = max(height, base_radius*4.0) * 2.2
else:
    cam.data.type = 'PERSP'
scene.camera = cam
scene.render.resolution_x = img_size[0]
scene.render.resolution_y = img_size[1]

# --- Create pyramid mesh (cone with 4 vertices => square base) ---
bpy.ops.mesh.primitive_cone_add(vertices=4, radius1=base_radius, radius2=0.0,
                                depth=height, enter_editmode=False, location=(0,0,0))
pyr = bpy.context.active_object
pyr.name = "Pyramid"
bpy.ops.object.shade_flat()

# --- Identify local coordinates for base vertices and tip (mesh in object local coords) ---
mesh = pyr.data
vertex_coords = [v.co.copy() for v in mesh.vertices]
zvals = [v.z for v in vertex_coords]
min_z = min(zvals)
max_z = max(zvals)

# base vertices: z close to min_z
base_vertex_indices = [i for i, v in enumerate(vertex_coords) if abs(v.z - min_z) < 1e-4]
if len(base_vertex_indices) < 4:
    # fallback: pick 4 vertices farthest from centroid in XY
    center = sum((v for v in vertex_coords), Vector((0,0,0))) / len(vertex_coords)
    dists = [(i, (vertex_coords[i] - center).length) for i in range(len(vertex_coords))]
    dists_sorted = sorted(dists, key=lambda x: -x[1])
    base_vertex_indices = [i for i, _ in dists_sorted[:4]]

base_pts = [vertex_coords[i] for i in base_vertex_indices]
base_pts_sorted = sorted(base_pts, key=lambda p: math.atan2(p.y, p.x))
base_mid_local = sum(base_pts_sorted, Vector((0.0,0.0,0.0))) / 4.0

# tip is vertex with maximum z
tip_idx = max(range(len(vertex_coords)), key=lambda i: vertex_coords[i].z)
tip_local = vertex_coords[tip_idx]

# --- Rotation helpers and model mapping (cone tip +Z -> model forward +X) ---
def Rx_deg(a): return Matrix.Rotation(math.radians(a), 4, 'X')
def Ry_deg(a): return Matrix.Rotation(math.radians(a), 4, 'Y')
def Rz_deg(a): return Matrix.Rotation(math.radians(a), 4, 'Z')
R_model = Ry_deg(90.0)  # maps mesh cone's +Z tip -> model-forward +X

# Helper: project world point to pixel coords (both BL and TL)
def world_to_pixel_coords(scene, camera, world_coord):
    # world_to_camera_view returns Vector((ndc_x, ndc_y, ndc_z)) with origin bottom-left
    ndc = world_to_camera_view(scene, camera, world_coord)
    px_bl = ndc.x * scene.render.resolution_x
    py_bl = ndc.y * scene.render.resolution_y
    # top-left (common labeling) = (px_bl, H - py_bl)
    px_tl = px_bl
    py_tl = scene.render.resolution_y - py_bl
    return (px_bl, py_bl, px_tl, py_tl, ndc.x, ndc.y, ndc.z)

# Small function to identify canonical test cases for sanity-check:
def check_sanity(pitch, yaw, roll, fwd):
    # We'll flag the three canonical tests with codes:
    # 1 -> p=0,y=0,r=0 -> tip should point right (fwd.x > 0, approx fwd.y~0,fwd.z~0)
    # 2 -> p=90,y=0,r=0 -> tip should point up (fwd.z > 0, approx fwd.x~0)
    # 3 -> p=-90,y=0,r=0 -> tip should point down (fwd.z < 0)
    if abs(pitch - 0) < 1e-6 and abs(yaw - 0) < 1e-6 and abs(roll - 0) < 1e-6:
        return 1 if fwd.x > 0 else 0
    if abs(pitch - 90) < 1e-6 and abs(yaw - 0) < 1e-6 and abs(roll - 0) < 1e-6:
        return 2 if fwd.z > 0 else 0
    if abs(pitch + 90) < 1e-6 and abs(yaw - 0) < 1e-6 and abs(roll - 0) < 1e-6:
        return 3 if fwd.z < 0 else 0
    return 0

# --- Iterate combos, compute world coords and pixel coords, write CSV ---
with open(CSVPATH, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    # header
    writer.writerow([
        "pitch.real","yaw.real","roll.real",
        # base world, pixels (bottom-left), pixels (top-left), ndc
        "base_wx","base_wy","base_wz",
        "base_px_bl","base_py_bl","base_px_tl","base_py_tl","base_ndc_x","base_ndc_y","base_ndc_z",
        # tip world and pixels
        "tip_wx","tip_wy","tip_wz",
        "tip_px_bl","tip_py_bl","tip_px_tl","tip_py_tl","tip_ndc_x","tip_ndc_y","tip_ndc_z",
        # forward vector (tip direction) and sanity flag
        "fwd_x","fwd_y","fwd_z","sanity_flag"
    ])

    count = 0
    for pitch in pitches:
        for yaw in yaws:
            for roll in rolls:
                # Construct rotation consistent with earlier scripts:
                # R_user = Rx(roll) @ Rz(yaw) @ Ry(-pitch)
                R_user =  Rx_deg(roll) @ Rz_deg(yaw) @ Ry_deg(-pitch)
                R_world = R_user @ R_model

                # Map local points to world positions (rotation-only)
                base_world = R_world @ base_mid_local
                tip_world = R_world @ tip_local

                # Project to pixel coordinates (both conventions)
                base_px_bl, base_py_bl, base_px_tl, base_py_tl, base_ndc_x, base_ndc_y, base_ndc_z = \
                    world_to_pixel_coords(scene, cam, base_world)
                tip_px_bl, tip_py_bl, tip_px_tl, tip_py_tl, tip_ndc_x, tip_ndc_y, tip_ndc_z = \
                    world_to_pixel_coords(scene, cam, tip_world)

                # forward vector (tip world direction) - since model forward was the cone tip vector
                fwd = R_world @ Vector((0.0, 0.0, 1.0))

                # sanity checks for a few canonical angles (helps debugging)
                sanity_flag = check_sanity(pitch, yaw, roll, fwd)

                writer.writerow([
                    pitch, yaw, roll,
                    base_world.x, base_world.y, base_world.z,
                    base_px_bl, base_py_bl, base_px_tl, base_py_tl, base_ndc_x, base_ndc_y, base_ndc_z,
                    tip_world.x, tip_world.y, tip_world.z,
                    tip_px_bl, tip_py_bl, tip_px_tl, tip_py_tl, tip_ndc_x, tip_ndc_y, tip_ndc_z,
                    fwd.x, fwd.y, fwd.z, sanity_flag
                ])
                count += 1

print(f"Wrote {count} rows to {CSVPATH}")
print("CSV:", CSVPATH)
